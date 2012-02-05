package elw.dao.rest;

import elw.dao.ctx.ScoresOfSolution;
import elw.vo.ScoreTerm;
import elw.vo.State;

/**
 * Score information of a file, with extended information.
 */
public class SolutionScore {
    public static SolutionScore create(ScoresOfSolution scores) {
        final SolutionScore score = new SolutionScore();

        score.solutionId = scores.solution.getId();
        score.scoreStamp = scores.score().getStamp();
        
        score.state = scores.state();
        score.terms = scores.terms();
        //  FIXME onsite/offsite and all other minor props
        score.sourceAddress = scores.solution.getSourceAddress();
        score.pointsBudget = scores.pointsBudget();
        if (scores.state() == State.APPROVED) {
            score.pointsApproved = scores.points();
        } else if (scores.state() == State.PENDING) {
            score.pointsPending = scores.points();
        }

        score.daysOverdue = scores.daysOverdue();
        score.daysOpen = scores.daysOpen();
        score.daysPending = scores.daysPending();

        return score;
    }

    private int daysOverdue;
    public int getDaysOverdue() { return daysOverdue; }

    private int daysOpen;
    public int getDaysOpen() { return daysOpen; }

    private int daysPending;
    public int getDaysPending() { return daysPending; }

    private String sourceAddress;
    public String getSourceAddress() { return sourceAddress; }

    private double pointsApproved;
    public double getPointsApproved() { return pointsApproved; }

    private double pointsPending;
    public double getPointsPending() { return pointsPending; }

    private double pointsBudget;
    public double getPointsBudget() { return pointsBudget; }

    private String solutionId;
    public String getSolutionId() { return solutionId; }

    private State state;
    public State getState() { return state; }

    private Long scoreStamp;
    public Long getScoreStamp() { return scoreStamp; }

    private ScoreTerm[] terms;
    public ScoreTerm[] getTerms() { return terms; }
}
