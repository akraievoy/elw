package elw.dao;

import elw.dao.ctx.CtxSolution;
import elw.dao.ctx.CtxStudent;

/**
 * Some code which filters out solution listings.
 */
public interface SolutionFilter {
    boolean preAllows(CtxStudent ctxStudent);
    boolean allows(CtxSolution ctxSolution);
}
