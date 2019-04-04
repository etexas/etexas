package com.harmonia.qa.par.Utilities.rules;

import java.util.concurrent.TimeUnit;

import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasProperties;
import com.harmonia.qa.webdriver.utilities.BasicWebDriverManager;

/**
 * Watches tests and gives useful debugging information.
 *
 * @author llaroussini
 */
public class ETexasTestWatcher extends TestWatcher implements ETexasProperties {

    /**
     * Logged sleep/wait time at beginning of test execution
     */
    private static long loggedSleepTimeAtStart = 0L;

    /*
     * (non-Javadoc)
     * @see org.junit.rules.TestWatcher#succeeded(org.junit.runner.Description)
     * Overrides JUnit's TestWatcher succeeded() method; called when a test
     * succeeds.
     */
    @Override
    protected void succeeded(Description d) {

    }

    /*
     * (non-Javadoc)
     * @see org.junit.rules.TestWatcher#failed(java.lang.Throwable,
     * org.junit.runner.Description) Overrides JUnit's TestWatcher failed()
     * method; called when a test fails.
     */
    @Override
    protected void failed(Throwable e, Description d) {

    }

    /*
     * (non-Javadoc)
     * @see org.junit.rules.TestWatcher#starting(org.junit.runner.Description)
     * Overrides JUnit's TestWatcher starting() method; called when a test
     * starts.
     */
    @Override
    protected void starting(Description description) {
        logStartTime(description);
    }

    /*
     * (non-Javadoc)
     * @see org.junit.rules.TestWatcher#finished(org.junit.runner.Description)
     * Overrides JUnit's TestWatcher finished() method; called when a test
     * finishes.
     */
    @Override
    protected void finished(Description description) {
        logEndTime(description);
    }

    /**
     * Logs the start time of a test.
     *
     * @param description the Description of the test
     */
    public void logStartTime(Description description) {
        String testName = getTestName(description);
        loggedSleepTimeAtStart = BasicWebDriverManager.get().getCurrentLoggedTime();
        printElapsedTime(testName + " : " + description.getMethodName() + " starting");
    }

    /**
     * Logs the end time of a test.
     *
     * @param description the Description of the test
     */
    public void logEndTime(Description description) {
        String testName = getTestName(description);
        long elapsedSleepTime = BasicWebDriverManager.get().getCurrentLoggedTime() - loggedSleepTimeAtStart;
        String sleepTime = String.format("%02d:%02d:%02d", TimeUnit.MILLISECONDS.toHours(elapsedSleepTime),
                TimeUnit.MILLISECONDS.toMinutes(elapsedSleepTime) - TimeUnit.HOURS.toMinutes(TimeUnit.MILLISECONDS.toHours(elapsedSleepTime)),
                TimeUnit.MILLISECONDS.toSeconds(elapsedSleepTime) - TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(elapsedSleepTime)));
        System.out.println("Time slept during " + description.getMethodName() + ": " + sleepTime);
        printElapsedTime(testName + " : " + description.getMethodName() + " ending");
    }

    /**
     * Returns the test name in a human readable format.
     *
     * @param description the Description of the test
     * @return the test name in a human readable format
     */
    private String getTestName(Description description) {
        String displayName = description.getDisplayName();
        String substring = displayName.substring(displayName.lastIndexOf(".") + 1, displayName.length());
        substring = substring.replace(")", "");
        return substring;
    }

    /**
     * Prints the amount of time elapsed since the start of the test suite.
     *
     * @param label the label to print with the time
     */
    public static void printElapsedTime(String label) {
        long currentTime = System.currentTimeMillis();
        long elapsedTime = currentTime - START_TIME;
        String time = String.format("%02d:%02d:%02d", TimeUnit.MILLISECONDS.toHours(elapsedTime),
                TimeUnit.MILLISECONDS.toMinutes(elapsedTime) - TimeUnit.HOURS.toMinutes(TimeUnit.MILLISECONDS.toHours(elapsedTime)),
                TimeUnit.MILLISECONDS.toSeconds(elapsedTime) - TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(elapsedTime)));
        System.out.println(label + " at: " + time);
    }
}