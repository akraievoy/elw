package elw.dao.rest;

import elw.dao.ctx.CtxSolution;
import elw.vo.ScoreTerm;
import elw.vo.State;

/**
 * Score information of a {@link elw.vo.Solution}, with extended information.
 */
public class RestScore extends RestScoreInput {
    public static RestScore create(CtxSolution scores) {
        if (scores == null) {
            return null;
        }

        final RestScore restScore = new RestScore();

        restScore.solutionId = scores.solution.getId();
        restScore.stampMillis = scores.score().getStamp();
        if (restScore.stampMillis != null) {
            restScore.stampNice = scores.dateTimeNice(restScore.stampMillis);
        }

        restScore.state = scores.state();
        restScore.terms = scores.terms();
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

    private double points;
    public double getPoints() { return points; }

    private State state;
    public State getState() { return state; }

    private Long stampMillis;
    public Long getStampMillis() { return stampMillis; }

    private String stampNice;
    public String getStampNice() { return stampNice; }

}
