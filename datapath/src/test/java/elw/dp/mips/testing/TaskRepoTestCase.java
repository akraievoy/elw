package elw.dp.mips.testing;

import junit.framework.TestCase;

public class TaskRepoTestCase extends TestCase {
    public void testFindAll() throws Exception {
        final Task[] all = TaskRepo.getInstance().findAll();

        assertEquals(84, all.length);
    }
}

