package elw.dp.mips.testing;

import org.akraievoy.gear.G4Io;
import org.akraievoy.gear.G4Str;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

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
			final String taskNumStr = "task" + G4Str.toPaddedString(taskNum, 3);

			final InputStream descProps = TaskRepo.class.getResourceAsStream(taskNumStr + "/desc.properties");
			if (descProps == null) {
				break;
			} else {
				if (descProps != null) {
					try {
						descProps.close();
					} catch (IOException e) {
						//	ignored
					}
				}
			}

			final Task newTask = new Task();

			newTask.setDescription(G4Io.getResource(TaskRepo.class, taskNumStr + "/desc.properties", ENC));
			newTask.setShortDesc(G4Io.getResource(TaskRepo.class, taskNumStr + "/short.properties", ENC));
			newTask.setSampleSolution(G4Io.getResource(TaskRepo.class, taskNumStr + "/sample.properties", ENC));

			for (int testNum = 0; testNum < 16; testNum++) {
				final String testNumStr = G4Str.toPaddedString(testNum, 2);

				final InputStream dataInProps = TaskRepo.class.getResourceAsStream(taskNumStr + "/" + testNumStr + "data-in.properties");
				if (dataInProps == null) {
					break;
				} else {
					if (dataInProps != null) {
						try {
							dataInProps.close();
						} catch (IOException e) {
							//	ignored
						}
					}
				}

				final TestingCase tCase = new TestingCase();

				tCase.setOrdinal(testNum + 1);
				tCase.setExecutionLimit(TestingCase.EXECUTION_LIMIT);

				tCase.setMemInput(G4Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "data-in.properties", ENC));
				tCase.setMemExpected(G4Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "data-out.properties", ENC));
				tCase.setRegsInput(G4Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "regs-in.properties", ENC));
				tCase.setRegsExpected(G4Io.getResource(TaskRepo.class, taskNumStr + "/" + testNumStr + "regs-out.properties", ENC));

				newTask.getCases().add(tCase);
			}

			loadedTasks.add(newTask);
		}

		return loadedTasks.toArray(new Task[loadedTasks.size()]);
	}


}
