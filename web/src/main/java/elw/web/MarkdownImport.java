package elw.web;

import com.google.common.base.Charsets;
import com.google.common.io.CharStreams;
import com.google.common.io.Files;
import org.akraievoy.couch.CouchDao;
import org.akraievoy.couch.Squab;
import elw.dao.Ctx;
import elw.dp.mips.TaskBean;
import elw.vo.*;
import org.springframework.context.support.FileSystemXmlApplicationContext;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MarkdownImport {
    protected static final Pattern PATTERN_LR_NO =
            Pattern.compile("# Комп.?ютерний практикум №(\\d+)");

    protected static final Pattern PATTERN_VER_START =
            Pattern.compile("## Варіанти завдань");

    protected static final Pattern PATTERN_VER_NO =
            Pattern.compile("^(\\d+)\\. ");

    protected static final Pattern PATTERN_VER_NAME =
            Pattern.compile("\\*\\*([^\\*]+)\\*\\*");

    protected static final Pattern PATTERN_TEST_NO =
            Pattern.compile("^\\s*\\* Приклад (\\d+):");

    public static void main(String[] args) throws IOException {
        //  you'll also need to comment out 'scope=provided' in web/pom.xml
        //      for this to work, as servlet api is not visible by default
        final FileSystemXmlApplicationContext appCtx =
            new FileSystemXmlApplicationContext(
                "file://" +
                "/home/w/Projects/github/elw" +
                "/web/src/main/webapp/WEB-INF/elw-mvc-servlet.xml"
            );
        appCtx.start(); //  finally: destroy the context

        final CouchDao metaDao =
                (CouchDao) appCtx.getBean("metaDao", CouchDao.class);
        final CouchDao attachmentDao =
                (CouchDao) appCtx.getBean("attachmentDao", CouchDao.class);

        final String fileName = args.length == 1 ? args[0] :
                "/home/w/Projects/github/aos-tasks/aos-tasks.md";

        final List<String> lines = CharStreams.readLines(Files.newReader(
                new File(fileName),
                Charsets.UTF_8
        ));

        final Course course = metaDao.findOne(Course.class, "aos_w11");
        final TaskType tType = course.getTaskTypes().get("lr");
        tType.setId("lr");
        final Ctx ctx = Ctx.forAssType(course, tType);

        Integer lrNo = null;
        boolean versStarted = false;
        Integer verNo = null;
        String statement = null;
        Integer testNo = null;
        String test = null;
        SortedMap<Integer, String> names = new TreeMap<Integer, String>();
        SortedMap<Integer, String> statements = new TreeMap<Integer, String>();
        SortedMap<Integer, String> tests = new TreeMap<Integer, String>();
        for (int i = 0, linesSize = lines.size(); i < linesSize; i++) {
            final String line = lines.get(i);
            final Matcher matcherLrNo = PATTERN_LR_NO.matcher(line);
            final Matcher matcherVerStart = PATTERN_VER_START.matcher(line);
            final Matcher matcherVerNo = PATTERN_VER_NO.matcher(line);
            final Matcher matcherTestNo = PATTERN_TEST_NO.matcher(line);
            boolean verMatch = false;
            boolean testMatch = false;
            if (i + 1 == lines.size() || line.equals("# Л.тература")) {
                if (
                    (lrNo != null && verNo != null)
                ) {
                    if (testNo != null && test != null) {
                        tests.put(testNo, test);
                    }
                    updateTests(attachmentDao, ctx, lrNo, verNo, tests);
                    updateTask(metaDao, ctx, lrNo, names);
                    updateStatements(attachmentDao, ctx, lrNo, statements);
                }
                tests.clear();
                names.clear();
                statements.clear();
                test = null;
                testNo = null;
                verNo = null;
                lrNo = null;
            } else if (matcherLrNo.find()) {
                if (
                    (lrNo != null && verNo != null)
                ) {
                    if (testNo != null && test != null) {
                        tests.put(testNo, test);
                    }
                    if (!tests.isEmpty()) {
                        updateTests(attachmentDao, ctx, lrNo, verNo, tests);
                    }
                    updateTask(metaDao, ctx, lrNo, names);
                    updateStatements(attachmentDao, ctx, lrNo, statements);
                }
                names.clear();
                statements.clear();
                tests.clear();

                lrNo = Integer.parseInt(matcherLrNo.group(1));
                versStarted = false;
                testNo = null;
                test = null;
            } else if (matcherVerStart.find()) {
                versStarted = true;
            } else if (versStarted && (verMatch = matcherVerNo.find())) {
                if (testNo != null && test != null) {
                    tests.put(testNo, test);
                    updateTests(attachmentDao, ctx, lrNo, verNo, tests);
                }
                tests.clear();

                verNo = Integer.parseInt(matcherVerNo.group(1));
                testNo = null;
                test = null;

                String verName = null;
                final Matcher matcherVerName = PATTERN_VER_NAME.matcher(line);
                if (matcherVerName.find()) {
                    verName = matcherVerName.group(1);
                    statement = line.substring(matcherVerName.end() + 1);
                } else {
                    verName = "Ver #" + verNo;
                    statement = line.substring(matcherVerNo.end() + 1);
                }
                names.put(verNo, verName);
            } else if (
                    versStarted &&
                    verNo != null &&
                    (testMatch = matcherTestNo.find())
            ) {
                if (statement != null) {
                    statements.put(verNo, statement);
                    statement = null;
                }
                if (testNo != null && test != null) {
                    tests.put(testNo, test);
                }
                testNo = Integer.parseInt(matcherTestNo.group(1));
                test = "";
            }

            if (versStarted && verNo != null) {
                if (testNo == null && !verMatch && !line.trim().isEmpty()) {
                    statement += "\n" + line.trim();
                } else if (
                    test != null &&
                    !testMatch &&
                    !line.trim().isEmpty()
                ) {
                    final TaskBean.Test testRes = TaskBean.parseTest(line);
                    if (!testRes.parseErrors.getLineToErrors().isEmpty()) {
                        System.err.println(
                            testRes.errors(
                                testRes.parseErrors.getLineToErrors()
                            )
                        );
                    }
                    if (!test.trim().isEmpty()) {
                        test += "\n" + line.trim();
                    } else {
                        test = line.trim();
                    }
                }
            }
        }
    }

    private static void updateTask(
            CouchDao metaDao, Ctx ctx, Integer lrNo,
            SortedMap<Integer, String> names
    ) {
        final Task task;
        final Task taskSome = metaDao.findSome(
                Task.class,
                ctx.getCourse().getId(),
                ctx.getAssType().getId(),
                String.valueOf("lr" + lrNo)
        );

        if (taskSome != null) {
            task = taskSome;
        } else {
            task = new Task(
                 new String[] {
                    ctx.getCourse().getId(),
                    ctx.getAssType().getId()
                 }
            );
            task.setId("lr" + lrNo);
        }

        final TreeMap<String, Version> versions =
                new TreeMap<String, Version>();

        for (Integer verId : names.keySet()) {
            versions.put(
                String.valueOf(verId),
                new Version().withName(names.get(verId))
            );
        }

        task.setName("ЛР №" + lrNo);
        task.setVersions(versions);

        metaDao.update(task);
    }

    private static void updateStatements(
            CouchDao attachmentDao, Ctx ctx, Integer lrNo,
            SortedMap<Integer, String> statements
    ) {
        for (Integer verId : statements.keySet()) {
            final Attachment attachment;
            final Attachment attachmentSome = attachmentDao.findLast(
                    Attachment.class,
                    ctx.getCourse().getId(),
                    ctx.getAssType().getId(),
                    "lr" + lrNo,
                    String.valueOf(verId),
                    "statement",
                    "statement.txt"
            );

            if (attachmentSome != null) {
                attachment = attachmentSome;
            } else {
                attachment = new Attachment();

                attachment.setAuthor("akraievoy");
                attachment.setComment("Import from markdown");
                final TreeMap<String, FileType> fileType =
                        new TreeMap<String, FileType>();
                fileType.put("plaintext", null);
                attachment.setFileType(fileType);
                attachment.setName("statement.txt");
                attachment.setSourceAddress("lynx");


                final Version ver = new Version().withId(String.valueOf(verId));
                final Task task = new Task().withId("lr" + lrNo);
                task.setVersions(IdNamed._.singleton(ver));
                ctx.getAssType().setTasks(IdNamed._.singleton(task));
                final Ctx ctxTask = ctx.extendTask(task);
                final Ctx ctxVer = ctxTask.extendVer(ver);

                attachment.setupPathElems(
                        ctxVer,
                        new FileSlot().withId("statement")
                );
            }

            final Squab.CouchFile couchFile = new Squab.CouchFile();

            couchFile.setContentType("text/plain");
            couchFile.setData(statements.get(verId).getBytes());
            couchFile.setLength((long) couchFile.getData().length);

            attachment.putCouchFile(
                    FileBase.CONTENT,
                    couchFile
            );

            attachmentDao.update(attachment, false);
        }
    }

    private static void updateTests(
            CouchDao attachmentDao, Ctx ctx, Integer lrNo, Integer verNo,
            SortedMap<Integer, String> tests
    ) {
        for (Integer testId : tests.keySet()) {
            final Attachment attachment;
            final Attachment attachmentSome = attachmentDao.findLast(
                    Attachment.class,
                    ctx.getCourse().getId(),
                    ctx.getAssType().getId(),
                    "lr" + lrNo,
                    String.valueOf(verNo),
                    "test",
                    "test." + testId + ".txt"
            );

            if (attachmentSome != null) {
                attachment = attachmentSome;
            } else {
                attachment = new Attachment();

                attachment.setAuthor("akraievoy");
                attachment.setComment("Import from markdown");
                final TreeMap<String, FileType> fileType =
                        new TreeMap<String, FileType>();
                fileType.put("plaintext", null);
                attachment.setFileType(fileType);
                attachment.setName("test." + testId + ".txt");
                attachment.setSourceAddress("lynx");


                final Version ver = new Version().withId(
                        String.valueOf(verNo)
                );
                final Task task = new Task().withId("lr" + lrNo);
                task.setVersions(IdNamed._.singleton(ver));
                ctx.getAssType().setTasks(IdNamed._.singleton(task));
                final Ctx ctxTask = ctx.extendTask(task);
                final Ctx ctxVer = ctxTask.extendVer(ver);

                attachment.setupPathElems(
                        ctxVer,
                        new FileSlot().withId("test")
                );
            }

            final Squab.CouchFile couchFile = new Squab.CouchFile();

            couchFile.setContentType("text/plain");
            couchFile.setData(tests.get(testId).getBytes());
            couchFile.setLength((long) couchFile.getData().length);

            attachment.putCouchFile(
                    FileBase.CONTENT,
                    couchFile
            );

            attachmentDao.update(attachment, false);
        }
    }
}
