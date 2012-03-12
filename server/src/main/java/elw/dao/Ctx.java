package elw.dao;

import elw.dao.ctx.CtxSlot;
import elw.dao.ctx.CtxTask;
import elw.vo.Class;
import elw.vo.*;
import org.akraievoy.gear.G4Parse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

public class Ctx implements elw.vo.Ctx {
    private static final Logger log = LoggerFactory.getLogger(Ctx.class);

    private static final char ELEM_ENR = 'e';
    private static final char ELEM_INDEX_ENTRY = 'i';
    private static final char ELEM_GROUP = 'g';
    private static final char ELEM_STUD = 's';
    private static final char ELEM_COURSE = 'c';
    private static final char ELEM_ASS_TYPE = 't';
    private static final char ELEM_ASS = 'a';
    private static final char ELEM_VER = 'v';

    private static final String order = "egscitav";

    private static final String SEP = "--";

    private final KeyVal<String, Enrollment> enr;
    private final KeyVal<String, Student> student;
    private final KeyVal<String, TaskType> assType;
    private final KeyVal<String, Task> ass;
    private final KeyVal<String, Version> ver;

    private final KeyVal<String, Course> course;
    private final KeyVal<String, Group> group;
    private final KeyVal<Integer, IndexEntry> indexEntry;

    private final Map<Character, KeyVal> elemToKeyVal = new TreeMap<Character, KeyVal>();

    public Ctx(
            String enrId, String groupId, String studId,
            String courseId, Integer index, String assTypeId, String assId, String verId
    ) {
        enr = new KeyVal<String, Enrollment>(Enrollment.class, enrId);
        student = new KeyVal<String, Student>(Student.class, studId);
        assType = new KeyVal<String, TaskType>(TaskType.class, assTypeId);
        ass = new KeyVal<String, Task>(Task.class, assId);
        ver = new KeyVal<String, Version>(Version.class, verId);

        course = new KeyVal<String, Course>(Course.class, courseId);
        group = new KeyVal<String, Group>(Group.class, groupId);
        indexEntry = new KeyVal<Integer, IndexEntry>(IndexEntry.class, index);

        elemToKeyVal.put(ELEM_ENR, enr);
        elemToKeyVal.put(ELEM_STUD, student);
        elemToKeyVal.put(ELEM_ASS_TYPE, assType);
        elemToKeyVal.put(ELEM_ASS, ass);
        elemToKeyVal.put(ELEM_VER, ver);
        elemToKeyVal.put(ELEM_COURSE, course);
        elemToKeyVal.put(ELEM_GROUP, group);
        elemToKeyVal.put(ELEM_INDEX_ENTRY, indexEntry);
    }

    protected Ctx() {
        this(null, null, null, null, null, null, null, null);
    }

    public Task getAss() {
        return ass.getValue();
    }

    public TaskType getAssType() {
        return assType.getValue();
    }

    public Course getCourse() {
        return course.getValue();
    }

    public Enrollment getEnr() {
        return enr.getValue();
    }

    public Group getGroup() {
        return group.getValue();
    }

    public Student getStudent() {
        return student.getValue();
    }

    public Version getVer() {
        return ver.getValue();
    }

    public String getAssTypeId() {
        return assType.getKey();
    }

    public IndexEntry getIndexEntry() {
        return indexEntry.getValue();
    }

    public int getIndex() {
        return indexEntry.getKey();
    }

    public Ctx removeAll(String elements) {
        final Ctx res = copy();
        for (char element : elements.toCharArray()) {
            final KeyVal keyVal = res.elemToKeyVal.get(element);
            if (keyVal != null) {
                keyVal.clear();
            } else {
                throw new IllegalArgumentException("no keyVal for element '" + element + "'");
            }
        }
        return res;
    }

    public Ctx retainAll(String elements) {
        final Ctx res = copy();
        for (char element : res.elemToKeyVal.keySet()) {
            final KeyVal keyVal = res.elemToKeyVal.get(element);
            if (elements.indexOf(element) >= 0) {
                continue;
            }
            keyVal.clear();
        }
        return res;
    }

    public static Ctx fromString(final String path) {
        if (path == null || path.trim().length() == 0) {
            return new Ctx();
        }

        final String[] comp = path.split(SEP);
        if (comp.length < 1) {
            return new Ctx();
        }
        if (comp.length == 1) {
            throw new IllegalArgumentException("invalid context: '" + path + "'");
        }

        final String format = comp[0];
        if (format.length() + 1 != comp.length) {
            throw new IllegalArgumentException("format does not match content: '" + path + "'");
        }

        Ctx ctx = new Ctx();
        for (char elem : order.toCharArray()) {
            if (format.indexOf(elem) >= 0) {
                final String elemKeyStr = comp[format.indexOf(elem) + 1];
                if (elem != ELEM_INDEX_ENTRY) {
                    //noinspection unchecked
                    ctx.elemToKeyVal.get(elem).setKey(elemKeyStr);
                    continue;
                }

                Integer index = G4Parse.parse(elemKeyStr, -1);
                if (index < 0) {
                    throw new IllegalArgumentException("invalid indexEntry: '" + path + "'");
                }
                //noinspection unchecked
                ctx.elemToKeyVal.get(elem).setKey(index);
            }
        }

        return ctx;
    }

    public static Ctx forCourse(final Course course) {
        if (course == null) {
            throw new IllegalArgumentException("course is null");
        }
        return new Ctx().extendCourse(course);
    }

    public static Ctx forEnr(final Enrollment enr) {
        if (enr == null) {
            throw new IllegalArgumentException("enr is null");
        }
        return new Ctx().extEnr(enr);
    }

    public static Ctx forAssType(final Course course, final TaskType assType) {
        if (assType == null) {
            throw new IllegalArgumentException("task type is null");
        }
        return forCourse(course).extendAssType(assType);
    }

    public static Ctx forAss(Course course, TaskType assType, Task ass) {
        if (ass == null) {
            throw new IllegalArgumentException("ass is null");
        }
        return forAssType(course, assType).extendTask(ass);
    }

    public String ei() {
        return norm("ecgi") + SEP + getEnr().getId() + SEP + getIndex();
    }

    public String es() {
        return norm("ecgs") + SEP + getEnr().getId() + SEP + getStudent().getId();
    }

    public Ctx resolve(Queries queries) {
        if (enr.isPending()) {
            extEnr(queries.enrollmentSome(enr.getKey()));
        }
        if (course.isPending()) {
            extCourse(queries.course(course.getKey()));
        }
        if (group.isPending()) {
            extGroup(queries.group(group.getKey()));
        }

        return resolve();
    }

    protected Ctx resolve() {    //	this does not involve DAO lookups at all
        if (group.isResolved() && student.isPending()) {
            student.setValue(group.getValue().getStudents().get(student.getKey()));
        }

        if (enr.isResolved() && indexEntry.isPending()) {
            final Integer index = indexEntry.getKey();
            final IndexEntry ieVal;
            if (index >= 0 && index < enr.getValue().getIndex().size()) {
                ieVal = enr.getValue().getIndex().get(index);
            } else {
                ieVal = null;
            }
            if (indexEntry.resolve(ieVal)) {
                assType.setKey(indexEntry.getValue().getTaskTypeId());
                ass.setKey(indexEntry.getValue().getTaskId());
            }
        }

        if (course.isResolved() && assType.isPending()) {
            assType.resolve(course.getValue().getTaskTypes().get(assType.getKey()));
        }

        if (assType.isResolved() && ass.isPending()) {
            ass.resolve(assType.getValue().getTasks().get(ass.getKey()));
        }

        if (student.isResolved() && ass.isResolved()) {
            final String verId = Nav.resolveVersion(
                    ass.getValue(),
                    indexEntry.getValue(),
                    group.getValue(),
                    student.getValue().getId()
            ).getId();

            ver.setKey(verId);
        }

        if (ass.isResolved() && ver.isPending()) {
            ver.setValue(ass.getValue().getVersions().get(ver.getKey()));
        }

        return this;
    }

    private Ctx extEnr(final Enrollment newEnr) {
        if (enr.resolve(newEnr)) {
            course.setKey(enr.getValue().getCourseId());
            group.setKey(enr.getValue().getGroupId());
        }
        return this;
    }

    public Ctx extGroup(final Group newGroup) {
        if (enr.isResolved() && !newGroup.getId().equals(enr.getValue().getGroupId())) {
            throw new IllegalArgumentException("enrollment/group mismatch");
        }
        group.resolve(newGroup);
        return this;
    }

    public Ctx extCourse(final Course newCourse) {
        if (enr.isResolved() && !newCourse.getId().equals(enr.getValue().getCourseId())) {
            throw new IllegalArgumentException("enrollment/course mismatch");
        }
        course.resolve(newCourse);
        return this;
    }

    public Ctx extStudent(final Student newStud) {
        if (!group.isResolved()) {
            throw new IllegalStateException("group not resolved");
        }
        if (group.getValue().getStudents().get(newStud.getId()) == null) {
            throw new IllegalArgumentException("group/student mismatch");
        }
        student.resolve(newStud);
        return resolve();
    }

    public Ctx extIndex(final int index) {
        if (!enr.isResolved()) {
            throw new IllegalStateException("enrollment not resolved");
        }
        if (index < 0 || index >= enr.getValue().getIndex().size()) {
            throw new IllegalArgumentException("enrollment/index entry mismatch: " + index);
        }
        indexEntry.setKey(index);
        return resolve();
    }

    private Ctx extAssType(final TaskType newType) {
        if (!course.isResolved()) {
            throw new IllegalStateException("course not set");
        }
        if (course.getValue().getTaskTypes().get(newType.getId()) == null) {
            throw new IllegalArgumentException("course/aType mismatch");
        }
        assType.resolve(newType);
        return resolve();
    }

    private Ctx extTask(final Task newTask) {
        if (!assType.isResolved()) {
            throw new IllegalStateException("taskType not set");
        }
        if (assType.getValue().getTasks().get(newTask.getId()) == null) {
            throw new IllegalArgumentException("taskType/task mismatch");
        }
        ass.resolve(newTask);
        return resolve();
    }

    public Ctx extVer(final Version newVer) {
        if (!ass.isResolved()) {
            throw new IllegalArgumentException("task not set");
        }
        if (ass.getValue().getVersions().get(newVer.getId()) == null) {
            throw new IllegalArgumentException("task/ver mismatch");
        }
        ver.resolve(newVer);
        return this;
    }

    public Ctx extendGroup(final Group newGroup) {
        return copy().extGroup(newGroup);
    }

    public Ctx extendCourse(final Course newCourse) {
        return copy().extCourse(newCourse);
    }

    public Ctx extendStudent(final Student newStud) {
        return copy().extStudent(newStud);
    }

    public Ctx extendIndex(final int index) {
        return copy().extIndex(index);
    }

    private Ctx extendAssType(final TaskType newType) {
        return copy().extAssType(newType);
    }

    public Ctx extendTask(final Task newTask) {
        return copy().extTask(newTask);
    }

    public Ctx extendVer(final Version newVer) {
        return copy().extVer(newVer);
    }

    private String getResolveState() {
        final StringBuilder res = new StringBuilder();

        for (char elem : order.toCharArray()) {
            if (elemToKeyVal.get(elem).isResolved()) {
                res.append((elem));
            }
        }

        return res.toString();
    }

    public String toString() {
        final String format = norm(getResolveState());

        if (format.isEmpty()) {
            return "";
        }

        final StringBuilder res = new StringBuilder(format);

        for (char comp : format.toCharArray()) {
            res.append(SEP);
            res.append(elemToKeyVal.get(comp).getKey());
        }

        return res.toString();
    }

    @SuppressWarnings({"RedundantIfStatement"})
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Ctx ctx = (Ctx) o;

        if (!ass.equals(ctx.ass)) return false;
        if (!assType.equals(ctx.assType)) return false;
        if (!course.equals(ctx.course)) return false;
        if (!enr.equals(ctx.enr)) return false;
        if (!group.equals(ctx.group)) return false;
        if (!indexEntry.equals(ctx.indexEntry)) return false;
        if (!student.equals(ctx.student)) return false;
        if (!ver.equals(ctx.ver)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = enr.hashCode();
        result = 31 * result + student.hashCode();
        result = 31 * result + assType.hashCode();
        result = 31 * result + ass.hashCode();
        result = 31 * result + ver.hashCode();
        result = 31 * result + course.hashCode();
        result = 31 * result + group.hashCode();
        result = 31 * result + indexEntry.hashCode();
        return result;
    }

    private String dump() {
        return "e:" + enr + " g:" + group + " s:" + student + " c:" + course + "; " +
                " i:" + indexEntry + " t:" + assType + " a:" + ass + " v:" + ver;
    }

    private Ctx copy() {
        final Ctx copy = new Ctx();

        copy.enr.copyOf(enr);
        copy.group.copyOf(group);
        copy.student.copyOf(student);
        copy.course.copyOf(course);
        copy.indexEntry.copyOf(indexEntry);
        copy.assType.copyOf(assType);
        copy.ass.copyOf(ass);
        copy.ver.copyOf(ver);

        return copy;
    }

    public boolean resolved(final String state) {
        for (char elem : state.toCharArray()) {
            if (!elemToKeyVal.get(elem).isResolved()) {
                return false;
            }
        }

        return true;
    }

    @SuppressWarnings({"SimplifiableIfStatement"})
    private static boolean isRedundant(String elemsBefore, char elemAfter) {
        if (elemsBefore.indexOf(ELEM_ENR) >= 0 && elemsBefore.indexOf(ELEM_STUD) >= 0 && elemsBefore.indexOf(ELEM_INDEX_ENTRY) >= 0 && elemAfter == ELEM_VER) {
            return true;
        }
        if (elemsBefore.indexOf(ELEM_ENR) >= 0 && (elemAfter == ELEM_COURSE || elemAfter == ELEM_GROUP)) {
            return true;
        }

        return elemsBefore.indexOf(ELEM_INDEX_ENTRY) >= 0 && (elemAfter == ELEM_ASS_TYPE || elemAfter == ELEM_ASS);
    }

    private static String removeRedundant(final String format) {
        boolean anyRedundant = false;
        boolean[] redundant = new boolean[format.length()];
        for (int afterPos = 1; afterPos < format.length(); afterPos++) {
            char after = format.charAt(afterPos);
            if (isRedundant(format.substring(0, afterPos), after)) {
                anyRedundant = redundant[afterPos] = true;
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

    private static String reorder(final String format) {
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

                if (order.indexOf(cur) > order.indexOf(next)) {
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

    private static String norm(final String state) {
        return removeRedundant(reorder(state));
    }

    public Class cFrom() {
        if (!resolved("eci")) {
            throw new IllegalStateException(this.toString());
        }

        return CtxTask.classForKey(
                getEnr().getClasses(),
                getIndexEntry().getClassFrom()
        );
    }

    public Class cDue(final String slotId) {
        if (!resolved("eci")) {
            throw new IllegalStateException(this.toString());
        }

        final Map<String, String> due = getIndexEntry().getClassDue();
        final String classDue = due == null ? null : due.get(slotId);
        if (classDue == null) {
            return null;
        }

        return CtxTask.classForKey(
                getEnr().getClasses(), classDue
        );
    }

    //	LATER move this to base.G4mat
    private static String renderBytes(byte[] checkSum) {
        final StringBuilder result = new StringBuilder();

        for (byte checkByte : checkSum) {
            result.append(Integer.toString((checkByte & 0xff) + 0x100, 16).substring(1));
        }

        return result.toString();
    }

    public static String digest(String text) {
        try {
            final MessageDigest md = MessageDigest.getInstance("SHA-1");
            md.update(text.getBytes("UTF-8"), 0, text.length());
            return renderBytes(md.digest());
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }
    }

    public String cmpNameNorm(Format f, FileSlot slot, FileBase file) {
        final Version ver = getVer();
        String result;
        try {
            final String normName = getEnr().getName() + "-" + getStudent().getName() + "--" +
                    getAss().getName() + (ver == null ? "" : "-" + ver.getName()) + "--" +
                    slot.getName() + "-" + f.format(file.getStamp(), "MMdd-HHmm");

            final String oriName = file.getName();
            final String oriExt;
            if (oriName != null && oriName.trim().length() > 0) {
                final int oriLastDot = oriName.lastIndexOf(".");
                oriExt = oriLastDot < 0 ? "" : oriName.substring(oriLastDot);
            } else {
                oriExt = ".txt";
            }

            final String normNameNoWs = normName.replaceAll("[\\s\\\\/]+", "_") + oriExt;

            result = URLEncoder.encode(
                    normNameNoWs,
                    "UTF-8"
            );
        } catch (UnsupportedEncodingException e1) {
            throw new IllegalStateException("UTF-8 is NOT supported?!");
        }

        return result;
    }

    public boolean checkRead(FileSlot slot, SortedMap<String, List<Solution>> filesStud) {
        final FileSlot fileSlot = getAssType().getFileSlots().get(slot.getId());

        for (String slotIdRA : fileSlot.getReadApprovals()) {
            if (!isApprovedAny(slotIdRA, filesStud)) {
                return false;
            }
        }

        return true;
    }

    public boolean checkWrite(FileSlot slot, SortedMap<String, List<Solution>> filesStud) {
        final FileSlot fileSlot = getAssType().getFileSlots().get(slot.getId());
        if (!fileSlot.isWritable()) {
            return false;
        }

        final List<String> writeApprovals = fileSlot.getWriteApprovals();
        for (String slotIdWA : writeApprovals) {
            if (!isApprovedAny(slotIdWA, filesStud)) {
                return false;
            }
        }

        return true;
    }

    public static boolean isApprovedAny(
            final String slotId,
            final Map<String, List<Solution>> filesStud
    ) {
        final List<Solution> files = filesStud.get(slotId);

        if (files == null || files.isEmpty()) {
            return false;
        }

        boolean approved = false;
        for (Solution s : files) {
            if (s.getScore() != null && s.getScore().state() == State.APPROVED) {
                approved = true;
            }
        }
        return approved;
    }

    public static boolean isDeclinedLast(
            String slotId,
            final Map<String, List<Solution>> filesStud
    ) {
        final List<Solution> files = filesStud.get(slotId);

        if (files == null || files.size() == 0) {
            return false;
        }

        final Solution s = files.get(files.size() - 1);
        return s.getScore() != null && !(s.getScore().state() == State.APPROVED);
    }

    public static boolean isPendingLast(
            String slotId,
            final Map<String, List<Solution>> filesStud
    ) {
        final List<Solution> files = filesStud.get(slotId);

        if (files == null || files.isEmpty()) {
            return false;
        }

        final Solution f = files.get(files.size() - 1);
        return f.getScore() == null;
    }
    
    public CtxSlot ctxSlot(final FileSlot slot) {
        return new CtxSlot(
                getEnr(), getGroup(), getStudent(), getCourse(),
                indexEntry.getKey(),
                getAss(), getAssType(), getVer(),
                slot
        );
    }

    protected class KeyVal<KeyType, ValueType> {
        private final java.lang.Class<ValueType> valueClass;

        private KeyType key;
        private ValueType value;

        public KeyVal(final java.lang.Class<ValueType> valueClass) {
            this(valueClass, null, null);
        }

        public KeyVal(final java.lang.Class<ValueType> valueClass, KeyType key) {
            this(valueClass, key, null);
        }

        public KeyVal(java.lang.Class<ValueType> valueClass, KeyType key, ValueType value) {
            this.key = key;
            this.value = value;
            this.valueClass = valueClass;
        }

        public KeyType getKey() {
            return key;
        }

        public void setKey(KeyType key) {
            this.key = key;
        }

        public ValueType getValue() {
            return value;
        }

        public void setValue(ValueType value) {
            this.value = value;
        }

        @SuppressWarnings("unchecked")
        public boolean resolve(ValueType value) {
            if (value == null) {
                log.warn("resolved " + valueClass.getSimpleName() + " to null: " + Ctx.this.dump());
                return false;
            }

            if (value instanceof IdNamed && !(value instanceof IndexEntry)) {
                final String valueKey = ((IdNamed) value).getId();
                if (key != null) {
                    if (!key.equals(valueKey)) {
                        throw new IllegalStateException(
                                "resolved " + valueClass.getSimpleName() + " to a different object: " + Ctx.this.dump()
                        );
                    }
                } else {
                    this.key = (KeyType) valueKey;
                }
            } else {
                if (key == null) {
                    throw new IllegalStateException(
                            "please set " + valueClass.getSimpleName() + " key first: " + Ctx.this.dump()
                    );
                }
            }

            this.value = value;

            return true;
        }

        public void copyOf(KeyVal<KeyType, ValueType> that) {
            this.key = that.key;
            this.value = that.value;    //	LATER possible aliasing here
        }

        public void clear() {
            this.key = null;
            this.value = null;
        }

        public boolean isInited() {
            return key != null;
        }

        public boolean isResolved() {
            return value != null;
        }

        public boolean isPending() {
            return isInited() && !isResolved();
        }

        @Override
        public String toString() {
            if (!isInited()) {
                return "?";
            }
            return "'" + String.valueOf(key) + "'" + (isResolved() ? "" : "?");
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            KeyVal val = (KeyVal) o;

            if (key != null ? !key.equals(val.key) : val.key != null) return false;
            return valueClass.equals(val.valueClass);

        }

        @Override
        public int hashCode() {
            int result = valueClass.hashCode();
            result = 31 * result + (key != null ? key.hashCode() : 0);
            return result;
        }
    }
}
