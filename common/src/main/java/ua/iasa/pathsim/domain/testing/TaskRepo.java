package ua.iasa.pathsim.domain.testing;

import com.bws.base.utils.*;
import java.io.InputStream;

import java.util.*;

public class TaskRepo {
    static final String ENC = "Windows-1251";

    static final TaskRepo instance = new TaskRepo();

    Task[] tasks;

    TaskRepo() {
    }

    public static TaskRepo getInstance() {
        return instance;
    }

    public Task[] findAll() {
        if (tasks == null) {
            tasks = loadAllTasks();
        }
        
        return tasks;
    }

    Task[] loadAllTasks() {
        List<Task> loadedTasks = new ArrayList<Task>();

        for (int taskNum = 0; taskNum < 84; taskNum++) {
            final String taskNumStr = "task" + Str.toPaddedString(taskNum, 3);

            final InputStream descProps = TaskRepo.class.getResourceAsStream(taskNumStr + "/desc.properties");
            if (descProps == null) {
                break;
            } else {
                Io.safeClose(descProps);
            }

            final Task newTask= new Task();

            newTask.setDescription(Io.getResource(TaskRepo.class, taskNumStr + "/desc.properties", ENC));
            newTask.setShortDesc(Io.getResource(TaskRepo.class, taskNumStr + "/short.properties", ENC));
            newTask.setSampleSolution(Io.getResource(TaskRepo.class, taskNumStr + "/sample.properties", ENC));

            for (int testNum = 0; testNum < 16; testNum++) {
                final String testNumStr = Str.toPaddedString(testNum, 2);

                final InputStream dataInProps = TaskRepo.class.getResourceAsStream(taskNumStr + "/" + testNumStr + "data-in.properties");
                if (dataInProps == null) {
                    break;
                } else {
                    Io.safeClose(dataInProps);
                }

                final TestingCase tCase = new TestingCase();

                tCase.setOrdinal(testNum + 1);
                tCase.setExecutionLimit(TestingCase.EXECUTION_LIMIT);

                tCase.setMemInput(Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "data-in.properties", ENC));
                tCase.setMemExpected(Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "data-out.properties", ENC));
                tCase.setRegsInput(Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "regs-in.properties", ENC));
                tCase.setRegsExpected(Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "regs-out.properties", ENC));

                newTask.getCases().add(tCase);
            }

            loadedTasks.add(newTask);
        }

        return loadedTasks.toArray(new Task[loadedTasks.size()]);
    }


}
