package elw.web;

import elw.dao.ctx.CtxSolution;
import elw.dao.ctx.CtxStudent;
import elw.vo.DueState;
import elw.vo.State;

import javax.servlet.http.HttpServletRequest;

/**
 * Solution filtering setup, with parsing and matching encapsulated.
 */
public class RestSolutionFilter implements elw.dao.SolutionFilter {
    public final DueState dueState;
    public final boolean dueStateInv;
    public final String slotId;
    public final boolean slotIdInv;
    public final State state;
    public final boolean stateInv;
    public final String studentId;
    public final boolean studentIdInv;
    public final String taskId;
    public final boolean taskIdInv;
    public final String taskTypeId;
    public final boolean taskTypeIdInv;
    public final String versionId;
    public final boolean versionIdInv;

    protected RestSolutionFilter(
            DueState dueState, boolean dueStateInv,
            String slotId, boolean slotIdInv,
            State state, boolean stateInv,
            String studentId, boolean studentIdInv,
            String taskId, boolean taskIdInv,
            String taskTypeId, boolean taskTypeIdInv,
            String versionId, boolean versionIdInv
    ) {
        this.dueState = dueState;
        this.dueStateInv = dueStateInv;
        this.slotId = slotId;
        this.slotIdInv = slotIdInv;
        this.state = state;
        this.stateInv = stateInv;
        this.studentId = studentId;
        this.studentIdInv = studentIdInv;
        this.taskId = taskId;
        this.taskIdInv = taskIdInv;
        this.taskTypeId = taskTypeId;
        this.taskTypeIdInv = taskTypeIdInv;
        this.versionId = versionId;
        this.versionIdInv = versionIdInv;
    }

    public static elw.dao.SolutionFilter fromRequest(
            HttpServletRequest req
    ) {
        final String dueStateStr = req.getParameter("dueState");
        final String slotIdStr = req.getParameter("slotId");
        final String stateStr = req.getParameter("state");
        final String studentIdStr = req.getParameter("studentId");
        final String taskIdStr = req.getParameter("taskId");
        final String taskTypeIdStr = req.getParameter("taskTypeId");
        final String versionIdStr = req.getParameter("versionId");

        boolean valid = true;

        final DueState dueState;
        final boolean dueStateInv;
        if (dueStateStr != null) {
            if (dueStateStr.startsWith("!")) {
                dueStateInv = true;
                dueState = EnumPropertyEditor.fromString(
                        dueStateStr.substring(1),
                        DueState.values()
                );
            } else {
                dueStateInv = false;
                dueState = EnumPropertyEditor.fromString(
                        dueStateStr, DueState.values()
                );
            }
            valid &= dueState != null;
        } else {
            dueState = null;
            dueStateInv = false;
        }

        final String slotId;
        final boolean slotIdInv;
        if (slotIdStr != null) {
            if (slotIdStr.startsWith("!")) {
                slotIdInv = true;
                slotId = slotIdStr.substring(1);
            } else {
                slotIdInv = false;
                slotId = slotIdStr;
            }
        } else {
            slotId = null;
            slotIdInv = false;
        }

        final State state;
        final boolean stateInv;
        if (dueStateStr != null) {
            if (dueStateStr.startsWith("!")) {
                stateInv = true;
                state = EnumPropertyEditor.fromString(
                        stateStr.substring(1),
                        State.values()
                );
            } else {
                stateInv = false;
                state = EnumPropertyEditor.fromString(
                        stateStr, State.values()
                );
            }
            valid &= state != null;
        } else {
            state = null;
            stateInv = false;
        }

        final String studentId;
        final boolean studentIdInv;
        if (studentIdStr != null) {
            if (studentIdStr.startsWith("!")) {
                studentIdInv = true;
                studentId = studentIdStr.substring(1);
            } else {
                studentIdInv = false;
                studentId = studentIdStr;
            }
        } else {
            studentId = null;
            studentIdInv = false;
        }

        final String taskId;
        final boolean taskIdInv;
        if (taskIdStr != null) {
            if (taskIdStr.startsWith("!")) {
                taskIdInv = true;
                taskId = taskIdStr.substring(1);
            } else {
                taskIdInv = false;
                taskId = taskIdStr;
            }
        } else {
            taskId = null;
            taskIdInv = false;
        }

        final String taskTypeId;
        final boolean taskTypeIdInv;
        if (taskTypeIdStr != null) {
            if (taskTypeIdStr.startsWith("!")) {
                taskTypeIdInv = true;
                taskTypeId = taskTypeIdStr.substring(1);
            } else {
                taskTypeIdInv = false;
                taskTypeId = taskTypeIdStr;
            }
        } else {
            taskTypeId = null;
            taskTypeIdInv = false;
        }

        final String versionId;
        final boolean versionIdInv;
        if (versionIdStr != null) {
            if (versionIdStr.startsWith("!")) {
                versionIdInv = true;
                versionId = versionIdStr.substring(1);
            } else {
                versionIdInv = false;
                versionId = versionIdStr;
            }
        } else {
            versionId = null;
            versionIdInv = false;
        }

        if (!valid) {
            return null;
        }

        return new RestSolutionFilter(
                dueState, dueStateInv,
                slotId, slotIdInv,
                state, stateInv,
                studentId, studentIdInv,
                taskId, taskIdInv,
                taskTypeId, taskTypeIdInv,
                versionId, versionIdInv
        );
    }

    public boolean preAllows(CtxStudent ctxStudent) {
        if (studentId != null) {
            if (studentIdInv ^ !studentId.equals(ctxStudent.student.getId())) {
                return false;
            }
        }

        return true;
    }

    public boolean allows(final CtxSolution ctx) {
        if (!preAllows(ctx)) {
            return false;
        }

        if (dueState != null) {
            if (dueStateInv ^ !ctx.dueApplies(dueState)) {
                return false;
            }
        }

        if (slotId != null) {
            if (slotIdInv ^ !slotId.equals(ctx.slot.getId())) {
                return false;
            }
        }

        if (state != null) {
            if (stateInv ^ ctx.state() != state) {
                return false;
            }
        }

        if (taskId != null) {
            if (taskIdInv ^ !taskId.equals(ctx.task.getId())) {
                return false;
            }
        }

        if (taskTypeId != null) {
            if (taskTypeIdInv ^ !taskTypeId.equals(ctx.tType.getId())) {
                return false;
            }
        }

        if (versionId != null) {
            if (versionIdInv ^ !versionId.equals(ctx.ver.getId())) {
                return false;
            }
        }

        return true;
    }
}
