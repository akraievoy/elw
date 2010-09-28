package elw.dao;

import elw.vo.*;
import org.akraievoy.gear.G4Parse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Map;
import java.util.TreeMap;

public class Ctx {
	private static final Logger log = LoggerFactory.getLogger(Ctx.class);

	public static final String STATE_NONE = "";
	public static final String STATE_G = "g";
	public static final String STATE_GS = "gs";
	public static final String STATE_ECG = "ecg";
	public static final String STATE_ECGS = "ecgs";
	public static final String STATE_C = "c";
	public static final String STATE_CIV = "civ";
	public static final String STATE_EGSCIV = "egsciv";

	public static final char ELEM_ENR = 'e';
	public static final char ELEM_INDEX_ENTRY = 'i';
	public static final char ELEM_GROUP = 'g';
	public static final char ELEM_STUD = 's';
	public static final char ELEM_COURSE = 'c';
	public static final char ELEM_ASS_TYPE = 't';
	public static final char ELEM_ASS = 'a';
	public static final char ELEM_VER = 'v';

	protected static final String order = "egscitav";
	protected static final Map<Character, Integer> elemToOrder = createElemToOrderMap();

	public static final String SEP = "--";

	protected final String enrId;
	protected final String courseId;
	protected final String groupId;
	protected final String studId;
	protected int index;
	protected final String assTypeId;
	protected final String assId;
	protected final String verId;
	protected final String initState;

	//	required elements resolved
	protected Enrollment enr;
	protected Student student;
	protected AssignmentType assType;
	protected Assignment ass;
	protected Version ver;
	protected String resolveState;
	protected Course course;
	protected Group group;
	protected IndexEntry indexEntry;

	public Ctx(
			final String initState,
			String enrId, String groupId, String studId,
			String courseId, int index, String assTypeId, String assId, String verId
	) {
		this.initState = initState;
		this.resolveState = "";

		this.enrId = enrId;
		this.groupId = groupId;
		this.studId = studId;
		this.courseId = courseId;
		this.index = index;
		this.assTypeId = assTypeId;
		this.assId = assId;
		this.verId = verId;
	}

	protected Ctx() {
		this(STATE_NONE, null, null, null, null, -1, null, null, null);
	}

	public static Ctx fromString(final String path) {
		if (path == null || path.trim().length() == 0) {
			return new Ctx();
		}

		final String[] comp = path.split(SEP);
		if (comp.length <= 1) {
			return new Ctx();
		}

		final String format = comp[0];
		if (format.length() + 1 != comp.length) {
			log.warn("format does not match content, NOT parsing: {}", path);
			return new Ctx();
		}

		final String initState = reorder(format);

		final String enrId;
		if (format.indexOf(ELEM_ENR) >= 0) {
			enrId = comp[format.indexOf(ELEM_ENR) + 1];
		} else {
			enrId = null;
		}

		final String courseId;
		if (format.indexOf(ELEM_COURSE) >= 0) {
			courseId = comp[format.indexOf(ELEM_COURSE) + 1];
		} else {
			courseId = null;
		}

		final String groupId;
		if (format.indexOf(ELEM_GROUP) >= 0) {
			groupId = comp[format.indexOf(ELEM_GROUP) + 1];
		} else {
			groupId = null;
		}

		final String studId;
		if (format.indexOf(ELEM_STUD) >= 0) {
			studId = comp[format.indexOf(ELEM_STUD) + 1];
		} else {
			studId = null;
		}

		final int index;
		if (format.indexOf(ELEM_INDEX_ENTRY) >= 0) {
			index = G4Parse.parse(comp[format.indexOf(ELEM_INDEX_ENTRY) + 1], -1);
			if (index < 0) {
				log.warn("path[{}] must be an integer: {}", format.indexOf(ELEM_INDEX_ENTRY) + 1, path);
			}
		} else {
			index = -1;
		}

		final String typeId;
		if (format.indexOf(ELEM_ASS_TYPE) >= 0) {
			typeId = comp[format.indexOf(ELEM_ASS_TYPE) + 1];
		} else {
			typeId = null;
		}

		final String assId;
		if (format.indexOf(ELEM_ASS) >= 0) {
			assId = comp[format.indexOf(ELEM_ASS) + 1];
		} else {
			assId = null;
		}

		final String verId;
		if (format.indexOf(ELEM_VER) >= 0) {
			verId = comp[format.indexOf(ELEM_VER) + 1];
		} else {
			verId = null;
		}

		return new Ctx(initState, enrId, groupId, studId, courseId, index, typeId, assId, verId);
	}

	public static Ctx forCourse(final Course course) {
		if (course == null) {
			throw new IllegalArgumentException("please provide the course for context");
		}
		return new Ctx().extendCourse(course);
	}

	public static Ctx forEnr(final Enrollment enr) {
		if (enr == null) {
			throw new IllegalArgumentException("please provide the enr for context");
		}
		return new Ctx().extendEnr(enr);
	}

	public static Ctx forAssType(final Course course, final AssignmentType assType) {
		if (course == null) {
			throw new IllegalArgumentException("please provide course for context");
		}
		if (assType == null) {
			throw new IllegalArgumentException("please provide assType for context");
		}
		return new Ctx().extendCourse(course).extendAssType(assType);
	}

	public static Ctx forAss(Course course, AssignmentType assType, Assignment ass) {
		if (course == null) {
			throw new IllegalArgumentException("please provide course for context");
		}
		if (assType == null) {
			throw new IllegalArgumentException("please provide assType for context");
		}
		if (ass == null) {
			throw new IllegalArgumentException("please provide ass for context");
		}
		return new Ctx().extendCourse(course).extendAssType(assType).extendAss(ass);
	}

	public Ctx resolve(EnrollDao enrDao, GroupDao groupDao, CourseDao courseDao) {
		final StringBuilder resolved = new StringBuilder();

		if (inited(ELEM_ENR)) {
			enr = enrDao.findEnrollment(enrId);
			if (enr != null) {
				resolved.append(ELEM_ENR);

				course = courseDao.findCourse(enr.getCourseId());
				if (course != null) {
					resolved.append(ELEM_COURSE);
				} else {
					log.warn("course {} not found: {}", enr.getCourseId(), dump());
				}

				group = groupDao.findGroup(enr.getGroupId());
				if (group != null) {
					resolved.append(ELEM_GROUP);
				} else {
					log.warn("group {} not found: {}", enr.getGroupId(), dump());
				}
			}

		}

		if (inited(ELEM_COURSE) && !has(resolved, ELEM_COURSE)) {
			course = courseDao.findCourse(courseId);
			if (course != null) {
				resolved.append(ELEM_COURSE);
			} else {
				log.warn("course {} not found: {}", courseId, dump());
			}
		}

		if (inited(ELEM_GROUP) && !has(resolved, ELEM_GROUP)) {
			group = groupDao.findGroup(groupId);
			if (group != null) {
				resolved.append(ELEM_GROUP);
			} else {
				log.warn("group {} not found: {}", groupId, dump());
			}
		}

		if (has(resolved, ELEM_GROUP) && inited(ELEM_STUD)) {
			student = IdName.findById(group.getStudents(), studId);
			if (student != null) {
				resolved.append(ELEM_STUD);
			} else {
				log.warn("student {} not found: {}", studId, dump());
			}
		}

		if (has(resolved, ELEM_ENR) && inited(ELEM_INDEX_ENTRY)) {
			if (index >= 0 && index < enr.getIndex().size()) {
				indexEntry = enr.getIndex().get(index);
				resolved.append(ELEM_INDEX_ENTRY);
				assType = IdName.findById(course.getAssTypes(), indexEntry.getPath()[0]);
				if (assType != null) {
					resolved.append(ELEM_ASS_TYPE);
				} else {
					log.warn("type not found: {}", assTypeId, dump());
				}
				ass = IdName.findById(assType.getAssignments(), indexEntry.getPath()[1]);
				if (ass != null) {
					resolved.append(ELEM_ASS);
				} else {
					log.warn("assignment not found: {}", dump());
				}
			} else {
				log.warn("task not found by index: {}", index, dump());
			}
		}

		if (has(resolved, ELEM_COURSE) && inited(ELEM_ASS_TYPE)) {
			assType = IdName.findById(course.getAssTypes(), assTypeId);
			if (assType != null) {
				resolved.append(ELEM_ASS_TYPE);
			} else {
				log.warn("type not found: {}", assTypeId, dump());
			}
		}

		if (has(resolved, ELEM_ASS_TYPE) && inited(ELEM_ASS)) {
			ass = IdName.findById(assType.getAssignments(), assId);
			if (ass != null) {
				resolved.append(ELEM_ASS);
			} else {
				log.warn("assignment not found: {}", dump());
			}
		}

		if (has(resolved, ELEM_ASS) && inited(ELEM_VER)) {
			ver = IdName.findById(ass.getVersions(), verId);
			if (ver != null && (student == null || !isVersionIncorrect(student, ass, ver))) {
				resolved.append(ELEM_VER);
			} else {
				log.warn("version not found: {}", verId, this);
			}
		}

		resolveState = resolved.toString();
		return this;
	}

	public static boolean isVersionIncorrect(Student student, Assignment ass, Version ver) {
		final int studId = Integer.parseInt(student.getId());
		final int verIdx = IdName.indexOfId(ass.getVersions(), ver.getId());

		return (studId) % ass.getVersions().length != verIdx;
	}

	public String toString() {
		final String format = norm(getResolveState());

		if (format.isEmpty()) {
			return "";
		}

		final StringBuilder res = new StringBuilder();
		res.append(format);

		for (int i = 0; i < format.length(); i++) {
			res.append(SEP);
			final char comp = res.charAt(i);
			if (comp == ELEM_ENR) {
				res.append(getEnr().getId());
			} else if (comp == ELEM_GROUP) {
				res.append(getGroup().getId());
			} else if (comp == ELEM_STUD) {
				res.append(getStudent().getId());
			} else if (comp == ELEM_COURSE) {
				res.append(getCourse().getId());
			} else if (comp == ELEM_INDEX_ENTRY) {
				res.append(getIndex());
			} else if (comp == ELEM_ASS_TYPE) {
				res.append(getAssType().getId());
			} else if (comp == ELEM_ASS) {
				res.append(getAss().getId());
			} else if (comp == ELEM_VER) {
				res.append(getVer().getId());
			}
		}

		return res.toString();
	}

	public String dump() {
		return
				"e:" + enrId + " " +
				"g:" + groupId + " " +
				"s:" + studId + " " +
				"c:" + courseId + " " +
				"t:" + assTypeId + " " +
				"a:" + assId + " " +
				"v:" + verId;
	}

	public String getInitState() {
		return initState;
	}

	public String getResolveState() {
		return resolveState;
	}

	public Assignment getAss() {
		return ass;
	}

	public AssignmentType getAssType() {
		return assType;
	}

	public Course getCourse() {
		return course;
	}

	public Enrollment getEnr() {
		return enr;
	}

	public Group getGroup() {
		return group;
	}

	public Student getStudent() {
		return student;
	}

	public Version getVer() {
		return ver;
	}

	public String getAssTypeId() {
		return assTypeId;
	}

	public IndexEntry getIndexEntry() {
		return indexEntry;
	}

	public int getIndex() {
		return index;
	}

	public Ctx extendEnr(final Enrollment enr) {
		final Ctx ctx = copy();

		if (enr != null) {
			ctx.enr = enr;
			if (ctx.resolveState.indexOf(ELEM_ENR) < 0) {
				ctx.resolveState += ELEM_ENR;
			}
		} else {
			log.warn("extending with no enrollment");
		}

		return ctx;
	}

	public Ctx extendGroup(final Group group) {
		final Ctx ctx = copy();

		if (group != null) {
			if (enr == null || enr.getGroupId().equals(group.getId())) {
				ctx.group = group;
				if (ctx.resolveState.indexOf(ELEM_GROUP) < 0) {
					ctx.resolveState += ELEM_GROUP;
				}
			} else {
				log.warn("extending with wrong group");
			}
		} else {
			log.warn("extending with no group");
		}

		return ctx;
	}

	public Ctx extendCourse(final Course course) {
		final Ctx ctx = copy();

		if (course != null) {
			if (enr == null || enr.getCourseId().equals(course.getId())) {
				ctx.course = course;
				if (ctx.resolveState.indexOf(ELEM_COURSE) < 0) {
					ctx.resolveState += ELEM_COURSE;
				}
			} else {
				log.warn("extending with wrong course");
			}
		} else {
			log.warn("extending with no course");
		}

		return ctx;
	}

	public Ctx extendStudent(final Student student) {
		final Ctx ctx = copy();

		if (student != null) {
			if (group != null && IdName.findById(group.getStudents(), student.getId()) != null) {
				ctx.student = student;
				if (ctx.resolveState.indexOf(ELEM_STUD) < 0) {
					ctx.resolveState += ELEM_STUD;
				}
			} else {
				log.warn("extending with wrong student (or no group)");
			}
		} else {
			log.warn("extending with no student");
		}

		return ctx;
	}

	public Ctx extendIndex(final int index) {
		final Ctx ctx = copy();

		if (index >= 0) {
			if (enr != null && enr.getIndex().size() > index) {
				ctx.index = index;
				ctx.indexEntry = enr.getIndex().get(index);
				if (ctx.resolveState.indexOf(ELEM_INDEX_ENTRY) < 0) {
					ctx.resolveState += ELEM_INDEX_ENTRY;
				}
				ctx.assType = IdName.findById(ctx.course.getAssTypes(), ctx.indexEntry.getPath()[0]);
				if (ctx.assType != null) {
					ctx.resolveState += ELEM_ASS_TYPE;
				} else {
					log.warn("assignment type not found: {}", assTypeId, dump());
				}
				ctx.ass = IdName.findById(ctx.assType.getAssignments(), ctx.indexEntry.getPath()[1]);
				if (ctx.ass != null) {
					ctx.resolveState += ELEM_ASS;
				} else {
					log.warn("assignment not found: {}", dump());
				}
			} else {
				log.warn("extending with wrong index (or no enr resolved)");
			}
		} else {
			log.warn("extending with negative index");
		}

		return ctx;

	}

	public Ctx extendAssType(final AssignmentType assType) {
		final Ctx ctx = copy();

		if (assType != null) {
			if (course != null && IdName.findById(course.getAssTypes(), assType.getId()) != null) {
				ctx.assType = assType;
				if (ctx.resolveState.indexOf(ELEM_ASS_TYPE) < 0) {
					ctx.resolveState += ELEM_ASS_TYPE;
				}
			} else {
				log.warn("extending with wrong assType (or no course resolved)");
			}
		} else {
			log.warn("extending with no assType");
		}

		return ctx;
	}

	public Ctx extendAss(final Assignment ass) {
		final Ctx ctx = copy();

		if (ass != null) {
			ctx.ass = ass;
			if (ctx.resolveState.indexOf(ELEM_ASS) < 0) {
				ctx.resolveState += ELEM_ASS;
			}
		} else {
			log.warn("extending with no assignment");
		}

		return ctx;
	}

	public Ctx extendVer(final Version ver) {
		final Ctx ctx = copy();

		if (ver != null) {
			if (ass != null && IdName.findById(ass.getVersions(), ver.getId()) != null) {
				ctx.ver = ver;
				if (ctx.resolveState.indexOf(ELEM_VER) < 0) {
					ctx.resolveState += ELEM_VER;
				}
			} else {
				log.warn("extending with wrong version (or no assignment)");
			}
		} else {
			log.warn("extending with no version");
		}

		return ctx;
	}

	public Ctx extendTAV(AssignmentType assType, Assignment ass, Version ver) {
		return extendAssType(assType).extendAss(ass).extendVer(ver);
	}

	protected String getAssDirName() {
		return getAssType().getId() + "." + getAss().getId() + "." + getVer().getId();
	}

	protected File getStudentRoot(File uploadsDir) {
		return new File(
				uploadsDir,
				"" + getCourse().getId() + "." + getGroup().getId() + "/"
				+ getStudent().getId() + "." + getStudent().getName() + "/"
		);
	}

	public Ctx copy() {
		final Ctx copy = new Ctx(initState, enrId, groupId, studId, courseId, -1, assTypeId, assId, verId);

		copy.resolveState = resolveState;
		copy.enr = enr;
		copy.group = group;
		copy.student = student;
		copy.course = course;
		copy.index = index;
		copy.indexEntry = indexEntry;
		copy.assType = assType;
		copy.ass = ass;
		copy.ver = ver;

		return copy;
	}

	public boolean resolved(final String state) {
		for (int i = 0, stateLength = state.length(); i < stateLength; i++) {
			if (!resolved(state.charAt(i))) {
				return false;
			}
		}

		return true;
	}

	public boolean resolved(char c) {
		return has(resolveState, c);
	}

	public boolean inited(char c) {
		return has(initState, c);
	}

	protected boolean has(StringBuilder resolved, final char elem) {
		return has(resolved.toString(), elem);
	}

	public static boolean has(final String state, char elem) {
		for (int i = 0, stateLen = state.length(); i < stateLen; i++) {
			final char c1 = state.charAt(i);
			if (c1 == elem) {
				return true;
			}
		}

		return false;
	}

	protected static TreeMap<Character, Integer> createElemToOrderMap() {
		final TreeMap<Character, Integer> elemToOrder = new TreeMap<Character, Integer>();

		for (int pos = 0; pos < order.length(); pos++) {
			elemToOrder.put(order.charAt(pos), pos);
		}

		return elemToOrder;
	}

	protected static boolean isRedundant(char elemBefore, char elemAfter) {
		if (elemBefore == ELEM_ENR) {
			return (elemAfter == ELEM_COURSE || elemAfter == ELEM_GROUP);
		}
		if (elemBefore == ELEM_INDEX_ENTRY) {
			return (elemAfter == ELEM_ASS_TYPE || elemAfter == ELEM_ASS);
		}
		return false;
	}

	protected static String removeRedundant(final String format) {
		boolean anyRedundant = false;
		boolean[] redundant = new boolean[format.length()];
		for (int beforePos = 0; beforePos < format.length(); beforePos++) {
			char before = format.charAt(beforePos);
			for (int afterPos = beforePos + 1; afterPos < format.length(); afterPos++) {
				char after = format.charAt(afterPos);
				if (isRedundant(before, after)) {
					anyRedundant = redundant[afterPos] = true;
				}
			}
		}

		if (!anyRedundant) {
			return format;
		}

		final StringBuilder result = new StringBuilder();
		result.append(format);
		for (int pos = redundant.length - 1; pos >= 0; pos--) {
			if (redundant[pos]) {
				result.deleteCharAt(pos);
			}
		}
		return result.toString();
	}

	protected static String reorder(final String format) {
		char[] result = null;
		for (int count = 0; count < format.length(); count++) {
			boolean reordered = false;
			for (int pos = 0; pos < format.length() - 1; pos++) {
				char cur;
				char next;
				if (result == null) {
					cur = format.charAt(pos);
					next = format.charAt(pos + 1);
				} else {
					cur = result[pos];
					next = result[pos + 1];
				}

				if (elemToOrder.get(cur) > elemToOrder.get(next)) {
					reordered = true;
					if (result == null) {
						result = format.toCharArray();
					}

					result[pos] = next;
					result[pos + 1] = cur;
				}
			}
			if (!reordered) {
				break;
			}
		}

		return result != null ? String.valueOf(result) : format;
	}

	public static String norm(final String state) {
		return removeRedundant(reorder(state));
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		Ctx ctx = (Ctx) o;

		if (assTypeId != null ? !assTypeId.equals(ctx.assTypeId) : ctx.assTypeId != null) return false;
		if (assId != null ? !assId.equals(ctx.assId) : ctx.assId != null) return false;
		if (courseId != null ? !courseId.equals(ctx.courseId) : ctx.courseId != null) return false;
		if (enrId != null ? !enrId.equals(ctx.enrId) : ctx.enrId != null) return false;
		if (groupId != null ? !groupId.equals(ctx.groupId) : ctx.groupId != null) return false;
		if (studId != null ? !studId.equals(ctx.studId) : ctx.studId != null) return false;
		if (verId != null ? !verId.equals(ctx.verId) : ctx.verId != null) return false;

		return true;
	}

	@Override
	public int hashCode() {
		int result = enrId != null ? enrId.hashCode() : 0;
		result = 31 * result + (courseId != null ? courseId.hashCode() : 0);
		result = 31 * result + (groupId != null ? groupId.hashCode() : 0);
		result = 31 * result + (studId != null ? studId.hashCode() : 0);
		result = 31 * result + (assTypeId != null ? assTypeId.hashCode() : 0);
		result = 31 * result + (assId != null ? assId.hashCode() : 0);
		result = 31 * result + (verId != null ? verId.hashCode() : 0);
		return result;
	}
}
