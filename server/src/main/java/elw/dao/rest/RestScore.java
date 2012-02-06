package elw.dao.rest;

import elw.dao.ctx.CtxSolution;
import elw.vo.ScoreTerm;
import elw.vo.State;

/**
 * Score information of a file, with extended information.
 */
public class RestScore {
    public static RestScore create(CtxSolution scores) {
        final RestScore restScore = new RestScore();

        restScore.solutionId = scores.solution.getId();
        restScore.scoreStamp = scores.score().getStamp();
        
        restScore.state = scores.state();
        restScore.terms = scores.terms();
        //  FIXME onsite/offsite and all other minor props
        restScore.sourceAddress = scores.solution.getSourceAddress();
        restScore.points = scores.pointsForSolution();

        restScore.daysOverdue = scores.daysOverdue();
        restScore.daysOpen = scores.daysOpen();
        restScore.daysPending = scores.daysPending();

        return restScore;
    }

    private int daysOverdue;
    public int getDaysOverdue() { return daysOverdue; }

    private int daysOpen;
    public int getDaysOpen() { return daysOpen; }

    private int daysPending;
    public int getDaysPending() { return daysPending; }

    private String sourceAddress;
    public String getSourceAddress() { return sourceAddress; }

    private double points;
    public double getPoints() { return points; }

    private String solutionId;
    public String getSolutionId() { return solutionId; }

    private State state;
    public State getState() { return state; }

    private Long scoreStamp;
    public Long getScoreStamp() { return scoreStamp; }

    private ScoreTerm[] terms;
    public ScoreTerm[] getTerms() { return terms; }
}
