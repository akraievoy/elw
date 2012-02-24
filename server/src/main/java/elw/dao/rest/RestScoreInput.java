package elw.dao.rest;

import elw.vo.ScoreTermInput;

/**
 * POST/PUT encoding of new {@link Score} information.
 */
public class RestScoreInput {
    protected String solutionId;
    public String getSolutionId() { return solutionId; }
    public void setSolutionId(String solutionId) {
        this.solutionId = solutionId;
    }

    protected ScoreTermInput[] terms;
    public ScoreTermInput[] getTerms() { return terms; }
    public void setTerms(ScoreTermInput[] terms) {
        this.terms = terms;
    }
}
