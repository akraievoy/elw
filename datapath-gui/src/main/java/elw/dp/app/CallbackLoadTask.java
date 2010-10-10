package elw.dp.app;

import elw.dp.mips.TaskBean;

/**
 * LATER add javadocs for a class created by anton
 */
public interface CallbackLoadTask {
	void setTask(TaskBean task);

	void updateStatus(String newStatus, Throwable fault);

	void onTaskLoadComplete();
}
