package elw.dao.ctx;

import elw.dao.Nav;
import elw.vo.*;
import elw.vo.Class;

import java.util.Map;
import java.util.TreeMap;

/**
 * Relatively harmless parameter object, storing the full Score context.
 */
public class ScoresOfSolution extends SolutionsOfSlot {
    public final Solution solution;
    
    public ScoresOfSolution(
            Enrollment enr,
            Group group,
            Student student,
            Course course,
            int idx,
            Task task,
            TaskType tType,
            Version ver,
            FileSlot slot,
            Solution solution
    ) {
        super(enr, group, student, course, idx, task, tType, ver, slot);
        this.solution = solution;
    }

    public Score preliminary() {
        final Class classDue = Nav.classDue(enr, idxEntry, slot);
        final Class classFrom = Nav.classFrom(enr, idxEntry);

        final Map<String, Double> vars = new TreeMap<String, Double>();

        final double overdueDays;
        if (classDue == null) {
            overdueDays = 0.0;
        } else {
            overdueDays = (double) classDue.computeDaysOverdue(solution);
        }
        final double onTime = flag(enr.checkOnTime(solution));
        //  LATER onSite may be different for different classes
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
}
