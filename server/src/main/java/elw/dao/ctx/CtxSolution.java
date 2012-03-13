package elw.dao.ctx;

import elw.vo.*;
import elw.vo.Class;

import java.util.Map;
import java.util.TreeMap;

/**
 * Parameter Object, storing the full Solution context.
 */
public class CtxSolution extends CtxSlot {
    public final Solution solution;
    
    public CtxSolution(
            Enrollment enr,
            Group group,
            Student student,
            Course course,
            IndexEntry indexEntry,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot,
            Solution solution
    ) {
        super(enr, group, student, course, indexEntry, task, tType, ver, slot);
        this.solution = solution;
    }

    public CtxScore lastScore() {
        return score(score());
    }

    public CtxScore score(final Score score) {
        final CtxScore ctxScore = new CtxScore(
                enr, group, student,
                course, indexEntry, task, tType, ver,
                slot, solution, score
        );

        return propagateTZCache(ctxScore);
    }

    public Score score() {
        return solution.getScore();
    }

    public Score preliminary() {
        final Class classDue = dueClass();
        final Class classFrom = openClass();

        final Map<String, Double> vars = new TreeMap<String, Double>();

        final double overdueDays;
        if (classDue == null) {
            overdueDays = 0.0;
        } else {
            overdueDays = (double) classDue.computeDaysOverdue(solution);
        }
        final double onTime = flag(enr.checkOnTime(solution));
        //  TODO onSite may be different for different classes
        final double onSite = flag(
                classFrom.checkOnSite(solution.getSourceAddress())
        );
        final double rapid = flag(classFrom.checkOnTime(solution));

        vars.put("$overdue", overdueDays);
        vars.put("$ontime", onTime);
        vars.put("$onsite", onSite);
        vars.put("$offtime", 1 - onTime);
        vars.put("$offsite", 1 - onSite);
        vars.put("$rapid", rapid);

        if (solution.getTotalTests() > 0 || solution.isValidated()) {
            vars.put("$passratio", solution.getPassRatio());
        }

        final Score preliminary = new Score();
        for (Criteria c : slot.getCriterias().values()) {
            if (preliminary.contains(slot, c)) {
                continue;
            }

            final Double ratio = c.resolveRatio(vars);
            final Integer powDef = c.resolvePowDef(vars);

            preliminary.register(slot, c, ratio, powDef);
        }

        return preliminary;
    }

    private double flag(final boolean theBool) {
        return theBool ? 1.0 : 0.0;
    }

    public int daysPending() {
        final Score score = score();
        final Long scoreStamp;
        if (score.state() != State.PENDING) {
            scoreStamp = score().getStamp();
        } else {
            scoreStamp = System.currentTimeMillis();
        }

        final int daysToApprove = days(openMillis(), scoreStamp);
        final int daysToUpload = days(openMillis(), solution.getStamp());

        return daysToApprove - daysToUpload;
    }

    public int daysOpen() {
        return days(openMillis(), solution.getStamp());
    }

    public int daysOverdue() {
        return days(dueMillis(), solution.getStamp());
    }

    public State state() {
        return score().state();
    }

    public double pointsForSolution() {
        return indexEntry.computePoints(score(), slot);
    }

    public ScoreTerm[] terms() {
        return score().getTerms(tType, false);
    }
}
