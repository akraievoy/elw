package elw.vo;

import org.codehaus.jackson.annotate.JsonIgnore;

import java.util.*;

public class Score extends Squab.Stamped implements Stamped {
    private Boolean approved;
    public Boolean getApproved() { return approved; }
    public void setApproved(Boolean approved) { this.approved = approved; }

    private String comment;
    public String getComment() { return comment; }
    public void setComment(String comment) { this.comment = comment; }

    private final Map<String, Double> ratios = new TreeMap<String, Double>();
    public Map<String, Double> getRatios() {
        return Collections.unmodifiableMap(ratios);
    }
    @SuppressWarnings({"UnusedDeclaration"})
    public void setRatios(Map<String, Double> ratios) {
        this.ratios.clear();
        if (ratios != null) {
            this.ratios.putAll(ratios);
        }
    }

    private final Map<String, Integer> pows = new TreeMap<String, Integer>();
    public Map<String, Integer> getPows() {
        return Collections.unmodifiableMap(pows);
    }
    public void setPows(Map<String, Integer> pows) {
        this.pows.clear();
        if (pows != null) {
            this.pows.putAll(pows);
        }
    }

    private boolean best;	//	transient
    @JsonIgnore
    public boolean isBest() { return best; }
    @JsonIgnore
    public void setBest(boolean best) { this.best = best; }

    public Score copy() {
        final Score copy = new Score();

        copy.ratios.putAll(ratios);
        copy.pows.putAll(pows);

        return copy;
    }

    public double getRatio(String[] ids) {
        double res = 1.0;

        for (final String id : ids) {
            if (!contains(id)) {
                continue;
            }
            res *= Math.pow(ratios.get(id), pows.get(id));
        }

        return res;
    }

    public double computeRatio(FileSlot slot) {
        double res = 1.0;

        for (final Criteria c : slot.getCriterias().values()) {
            final String id = idFor(slot, c);
            if (!contains(id)) {
                continue;
            }
            res *= Math.pow(ratios.get(id), pows.get(id));
        }

        return res;
    }

    public String idFor(FileSlot slot, Criteria c) {
        return slot.getId() + "--" + c.getId();
    }

    private boolean contains(String id) {
        return pows.containsKey(id) && ratios.containsKey(id);
    }

    public boolean containsAll(String[] ids) {
        for (final String id : ids) {
            if (!contains(id)) {
                return false;
            }
        }

        return true;
    }

    public boolean contains(FileSlot slot, Criteria c) {
        return contains(idFor(slot, c));
    }

    public int getPow(FileSlot slot, Criteria c) {
        return contains(slot, c) ? getPows().get(idFor(slot, c)) : 0;
    }

    public double getRatio(FileSlot slot, Criteria c) {
        return contains(slot, c) ? getRatios().get(idFor(slot, c)) : 1;
    }

    @JsonIgnore
    public ScoreTerm[] getTerms(TaskType aType, final boolean includeIdentity) {
        final List<ScoreTerm> scoreTerms = new ArrayList<ScoreTerm>();

        for (FileSlot slot : aType.getFileSlots().values()) {
            for (final Criteria c : slot.getCriterias().values()) {
                final String id = idFor(slot, c);
                if (contains(slot, c)) {
                    final Integer pow = pows.get(id);
                    final Double ratio = ratios.get(id);
                    final double termRatio = Math.pow(ratio, pow);

                    final ScoreTerm scoreTerm = new ScoreTerm(id, termRatio, pow, slot, c);

                    if (scoreTerm.isIdentity() && !includeIdentity) {
                        continue;
                    }

                    scoreTerms.add(scoreTerm);
                }
            }
        }

        return scoreTerms.toArray(new ScoreTerm[scoreTerms.size()]);
    }

    protected String[] extraPathElems = null;

    @Override
    protected String[] pathElems() {
        if (extraPathElems == null || extraPathElems.length != 9) {
            throw new IllegalStateException(
                    "pathElems: " +
                            "groupId" + PATH_SEP + // 0
                            "studId" + PATH_SEP + // 1
                            "courseId" + PATH_SEP + // 2
                            "ctxIdx" + PATH_SEP + // 3
                            "tTypeId" + PATH_SEP + // 4
                            "taskId" + PATH_SEP + // 5
                            "verId" + PATH_SEP + // 6
                            "slotId" + PATH_SEP + // 7
                            "solutionId" // 8
            );
        } else {
            return new String[] {
                    extraPathElems[0], extraPathElems[1],
                    extraPathElems[2], extraPathElems[3],
                    extraPathElems[4], extraPathElems[5],
                    extraPathElems[6], extraPathElems[7],
                    extraPathElems[8]
            };
        }
    }

    public void setupPathElems(Ctx ctx, FileSlot slot, Solution solution) {
        extraPathElems = new String[] {
                ctx.getGroup().getId(),
                ctx.getStudent().getId(),
                ctx.getCourse().getId(),
                String.valueOf(ctx.getIndex()),
                ctx.getAssType().getId(),
                ctx.getAss().getId(),
                ctx.getVer().getId(),
                slot.getId(),
                solution.getId()
        };
    }
}
