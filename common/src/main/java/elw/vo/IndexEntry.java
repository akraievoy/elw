package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

//  FIXME indexEntries should be stored in a treemap also,
//      where id may be derived from path and classFrom
public class IndexEntry implements Cloneable {
    //  FIXME add the index property

    private String[] path;
    public String[] getPath() { return path; }
    public void setPath(String[] path) { this.path = path; }

    private int scoreBudget;
    public int getScoreBudget() { return scoreBudget; }
    public void setScoreBudget(int scoreBudget) { this.scoreBudget = scoreBudget; }

    private String classFrom;
    public String getClassFrom() { return classFrom; }
    public void setClassFrom(String classFrom) { this.classFrom = classFrom; }

    private Map<String, String> classDue = new TreeMap<String, String>();
    public Map<String, String> getClassDue() {
        return Collections.unmodifiableMap(classDue);
    }
    public void setClassDue(Map<String, String> classDue) {
        this.classDue.clear();
        if (classDue != null) {
            this.classDue.putAll(classDue);
        }
    }

    private int verAnchor = 0;
    public int getVerAnchor() { return verAnchor; }
    public void setVerAnchor(int verAnchor) { this.verAnchor = verAnchor; }

    private int verStep = 1;
    public int getVerStep() { return verStep; }
    public void setVerStep(int verStep) { this.verStep = verStep; }

    //	TODO still not used
    private boolean requireClean;
    public boolean isRequireClean() { return requireClean; }
    public void setRequireClean(boolean requireClean) { this.requireClean = requireClean; }

    //	TODO pass Ctx to this place somehow
    public String normName(final Enrollment enr, final Student stud, final Task ass, final Version ver,
                           final FileSlot slot, final Solution meta, final Format format) {
        try {
            final String normName = enr.getName() + "-" + stud.getName() + "--" +
                    ass.getName() + (ver == null ? "" : "-" + ver.getName()) + "--" +
                    slot.getName() + "-" + format.format(meta.getStamp(), "MMdd-HHmm");

            final String oriName = meta.getName();
            final int oriLastDot = oriName.lastIndexOf(".");
            final String oriExt = oriLastDot < 0 ? "" : oriName.substring(oriLastDot);

            final String normNameNoWs = normName.replaceAll("[\\s\\\\/]+", "_") + oriExt;

            return URLEncoder.encode(
                    normNameNoWs,
                    "UTF-8"
            );
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException("UTF-8 is NOT supported?!");
        }
    }

    @JsonIgnore
    public double computePoints(Score score, final FileSlot slot) {
        if (Boolean.FALSE.equals(score.getApproved())) {
            return 0.0;
        }
        return getScoreBudget() * slot.getScoreWeight() * score.computeRatio(slot);
    }

    @JsonIgnore
    public double computePoints(final FileSlot slot) {
        return getScoreBudget() * slot.getScoreWeight();
    }

    @JsonIgnore
    public double getTotal(final TaskType aType, Score score) {
        double result = 0.0;

        for (FileSlot slot : aType.getFileSlots().values()) {
            if (!Boolean.TRUE.equals(score.getApproved())) {
                continue;
            }

            result += computePoints(score, slot);
        }

        return result;
    }

    @Override
    public IndexEntry clone() throws CloneNotSupportedException {
        IndexEntry clone = (IndexEntry) super.clone();
        
        clone.classDue = new TreeMap<String, String>(this.classDue);
        clone.path = this.path.clone();
        
        return clone;
    }
}
