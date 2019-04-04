package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.Execution;
import com.harmonia.qa.ETEXASWebQATests.entities.LaneChangeCommand;
import com.harmonia.qa.ETEXASWebQATests.entities.SignalChangeCommand;
import com.harmonia.qa.ETEXASWebQATests.entities.SpeedCommand;
import com.harmonia.qa.ETEXASWebQATests.entities.VehicleInjectionCommand;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * The partial page displayed as a part of the Execution Details page when the
 * Command History tab is selected
 *
 * @author llaroussini
 */
public class CommandHistoryPartialPage extends ExecutionPartialPage {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public CommandHistoryPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * App Log header cell titles
     *
     * @author llaroussini
     */
    public enum CommandHistoryHeaderCells {
        /**
         * The Time title for header cell
         */
        TIME("Time"),
        /**
         * The Command title for header cell
         */
        COMMAND("Command");

        /**
         * The label of the titles of the header cells as they appear in the
         * application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        CommandHistoryHeaderCells(String label) {
            this.label = label;
        }

        /**
         * Gets the label (title) associated with the header cell as it is
         * displayed in the Web UI
         *
         * @return The label of the header cell
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath prefix to command history column header cells
     */
    private static final String COMMAND_HISTORY_COLUMN_HEADER_CELL_XPATH_PREFIX = "//div[contains(@id, 'commandHistoryTable')]//span[text()='";

    /**
     * Xpath the the Command header cell
     */
    private static final String COMMAND_HEADER_CELL_XPATH = COMMAND_HISTORY_COLUMN_HEADER_CELL_XPATH_PREFIX + CommandHistoryHeaderCells.COMMAND.label + COLUMN_HEADER_CELL_XPATH_SUFFIX;

    /**
     * Xpath prefix to specific command row
     */
    private static final String COMMAND_ROW_XPATH_PREFIX = "//div[contains(@id, 'commandHistoryTable')]//div[contains(text(), '";

    /**
     * Xpath suffix to specific row in command queue
     */
    private static final String COMMAND_ROW_XPATH_SUFFIX = "')]/ancestor::tr";

    /**
     * Xpath to get command cell within a row
     */
    private static final String COMMAND_CELL_XPATH = "./td[contains(@class, 'x-grid-cell-last')]/div";

    /**
     * Xpath to get time cell within a row
     */
    private static final String TIME_CELL_XPATH = "./td[contains(@class, 'x-grid-cell-first')]/div";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the given column header cell
     *
     * @param headerCell -the header cell to get
     * @return the cell element
     */
    private El getHeaderCell(CommandHistoryHeaderCells headerCell) {
        return el(By.xpath(COMMAND_HISTORY_COLUMN_HEADER_CELL_XPATH_PREFIX + headerCell.label + COLUMN_HEADER_CELL_XPATH_SUFFIX));
    }

    /**
     * Gets command row containing given text
     *
     * @param text -the text expected in a row
     * @return the row element
     */
    private El getCommandRow(String text) {
        return el(By.xpath(COMMAND_ROW_XPATH_PREFIX + text + COMMAND_ROW_XPATH_SUFFIX));
    }

    /**
     * Gets command cell within given row
     *
     * @param row -the row to search
     * @return the command cell in the given row
     */
    private El getCommandCell(El row) {
        return row.el(By.xpath(COMMAND_CELL_XPATH));
    }

    /**
     * Gets time cell within given row
     *
     * @param row -the row to search
     * @return the time cell in the given row
     */
    private El getTimeCell(El row) {
        return row.el(By.xpath(TIME_CELL_XPATH));
    }

    /**
     * Gets the vehicle ID associated with the given Vehicle Injection command
     *
     * @param command -the vehicle injection command
     * @return the vehicle ID
     */
    public String getVehicleIDFromVehicleInjectionCommand(VehicleInjectionCommand command) {
        String commandName = command.getCommandType().getLabel();
        El row = getCommandRow(commandName);
        String displayedText = getCommandCell(row).getText();
        return displayedText.substring(39, 41);
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks if given header cell is displayed
     *
     * @param headerCell -the column header cell expected
     * @return true if displayed, false if not found or not visible
     */
    public boolean isHeaderCellDisplayed(CommandHistoryHeaderCells headerCell) {
        return isElementDisplayed(getHeaderCell(headerCell));
    }

    /**
     * Checks to see if command row is displayed
     *
     * @param text -the text expected in the row
     * @return true if displayed, false if not
     */
    public boolean isCommandRowDisplayed(String text) {
        try {
            El row = getCommandRow(text);
            return row != null; //Note: despite being present on the page the isDisplayed() call returns false.
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if command cell is present in the given row
     *
     * @param row -the row to search
     * @return true if cell is displayed, false otherwise
     */
    private boolean isCommandCellDisplayed(El row) {
        return isElementDisplayed(getCommandCell(row));
    }

    /**
     * Checks if time cell is present in the given row
     *
     * @param row -the row to search
     * @return true if cell is displayed, false otherwise
     */
    private boolean isTimeCellDisplayed(El row) {
        return isElementDisplayed(getTimeCell(row));
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Checks for presence of all column header cells (Time, Command)
     */
    public void checkAllColumnHeaderCells() {
        Assert.assertTrue("Time column header cell could not be found or is not visible.", isHeaderCellDisplayed(CommandHistoryHeaderCells.TIME));
        Assert.assertTrue("Command column header cell could not be found or is not visible.", isHeaderCellDisplayed(CommandHistoryHeaderCells.COMMAND));
    }

    /**
     * Checks execution for speed change commands, if present verifies command
     * rows display -- if no commands, verified command rows are not displayed
     *
     * @param execution -the execution to check for speed commands
     */
    public void checkSpeedCommandsDisplayed(Execution execution) {
        List<SpeedCommand> commands = execution.getSpeedCommands();
        if (commands != null) {
            String commandName = commands.get(0).getCommandType().getLabel();
            Assert.assertTrue("Speed Change commands for given execution could not be found.", isCommandRowDisplayed(commandName));
        }
        else {
            Assert.assertNull("Speed Change commands for given execution were not null.", commands);
        }
    }

    /**
     * Checks execution for lane change commands, if present verifies command
     * rows display -- if no commands, verified command rows are not displayed
     *
     * @param execution -the execution to check for lane change commands
     */
    public void checkLaneCommandsDisplayed(Execution execution) {
        List<LaneChangeCommand> commands = execution.getLaneChangeCommands();
        if (commands != null) {
            String commandName = commands.get(0).getLaneChangeCommandType().getLabel();
            Assert.assertTrue("Lane Change commands for given execution could not be found.", isCommandRowDisplayed(commandName));
        }
        else {
            Assert.assertNull("Lane Change commands for given execution were not null.", commands);
        }
    }

    /**
     * Checks details of given lane change command - verifies lane change
     * command row displays, command detail cell displays with expected info
     * (vehicle ID) and verified sim time cell displays in row with expected sim
     * time (the time the command was injected)
     *
     * @param command -the lane change command expected
     * @param simTime -the time the command was injected into the execution
     */
    public void checkLaneCommand(LaneChangeCommand command, String simTime) {
        String laneChangeCommandType = command.getLaneChangeCommandType().getLabel();
        Assert.assertTrue("The row for the expected lane change command, " + laneChangeCommandType + ", could not be found.", isCommandRowDisplayed(laneChangeCommandType));
        El row = getCommandRow(laneChangeCommandType);
        Assert.assertTrue("The command cell for the expected lane change command, " + laneChangeCommandType + ", could not be found.", isCommandCellDisplayed(row));
        String displayedText = getCommandCell(row).getText();
        String id = command.getVehicleID();
        Assert.assertTrue("The expected Vehicle ID of " + id + " could not be found in the row of command, " + laneChangeCommandType + ".", displayedText.contains(id));
        Assert.assertTrue("The time cell for the expected lane change command, " + laneChangeCommandType + ", could not be found.", isTimeCellDisplayed(row));
        String displayedTime = getTimeCell(row).getText().substring(0, 2);
        Assert.assertEquals("The expected sim time injection value of " + simTime + " could not be found in the row of command, " + laneChangeCommandType + ".", simTime, displayedTime);
    }

    /**
     * Checks details of given command - verifies speed command row displays,
     * command detail cell displays with expected info (vehicle ID and speed)
     * and verified sim time cell displays in row with expected sim time (the
     * time the command was injected)
     *
     * @param command -the command expected
     * @param simTime -the time the command was injected into the execution
     */
    public void checkSpeedCommand(SpeedCommand command, String simTime) {
        String speedCommand = command.getSpeedChangeCommand().getLabel();
        Assert.assertTrue("The row for the expected speed command, " + speedCommand + ", could not be found.", isCommandRowDisplayed(speedCommand));
        El row = getCommandRow(speedCommand);
        Assert.assertTrue("The command cell for the expected speed command, " + speedCommand + ", could not be found.", isCommandCellDisplayed(row));
        String displayedText = getCommandCell(row).getText();
        String id = command.getVehicleID();
        String speed = command.getSpeed();
        Assert.assertTrue("The expected Vehicle ID of " + id + " could not be found in the row of command, " + speedCommand + ".", displayedText.contains(id));
        Assert.assertTrue("The expected speed value of " + speed + " could not be found in the row of command, " + speedCommand + ".", displayedText.contains(speed));
        Assert.assertTrue("The time cell for the expected speed command, " + speedCommand + ", could not be found.", isTimeCellDisplayed(row));
        String displayedTime = getTimeCell(row).getText();
        Assert.assertEquals("The expected sim time injection value of " + simTime + " could not be found in the row of command, " + speedCommand + ".", simTime, displayedTime);
    }

    /**
     * Checks details of given command - verifies signal command row displays,
     * command detail cell displays with expected info (signal change command
     * type) and verifies sim time cell displays in row with expected sim time
     * (the time the command was injected)
     *
     * @param command -the command expected
     * @param simTime -the time the command was injected into the execution
     */
    public void checkSignalCommand(SignalChangeCommand command, String simTime) {
        String signalCommand = command.getSignalChangeCommandType().getLabel();
        Assert.assertTrue("The row for the expected signal command, " + signalCommand + ", could not be found.", isCommandRowDisplayed(signalCommand));
        El row = getCommandRow(signalCommand);
        Assert.assertTrue("The command cell for the expected signal command, " + signalCommand + ", could not be found.", isCommandCellDisplayed(row));
        String displayedText = getCommandCell(row).getText();
        String commandType = command.getSignalChangeCommandType().getLabel();
        Assert.assertTrue("The expected command type of " + commandType + " could not be found in the row of command, " + signalCommand + ".", displayedText.contains(commandType));
        Assert.assertTrue("The sim time cell for the expected signal command, " + signalCommand + ", could not be found.", isTimeCellDisplayed(row));
        String displayedTime = getTimeCell(row).getText();
        Assert.assertEquals("The expected sim time injection value of " + simTime + " could not be found in the row of command, " + signalCommand + ".", simTime, displayedTime);
    }

    /**
     * Checks details of given Vehicle Injection command - verifies command
     * displays with related info - lane, vehicle type, speed, and sim time when
     * command was injected
     *
     * @param command -the vehicle injection command expected
     * @param simTime -the sim time when command was expected to be injected
     */
    public void checkVehicleInjectionCommand(VehicleInjectionCommand command, String simTime) {
        String commandName = command.getCommandType().getLabel();
        Assert.assertTrue("The row for the expected vehicle injection command, " + commandName + ", could not be found.", isCommandRowDisplayed(commandName));
        El row = getCommandRow(commandName);
        Assert.assertTrue("The command cell for the expected signal command, " + commandName + ", could not be found.", isCommandCellDisplayed(row));
        String displayedText = getCommandCell(row).getText();
        String lane = command.getLane();
        Assert.assertTrue("The expected lane of " + lane + " could not be found in the row of command, " + commandName + ".", displayedText.contains(lane));
        String vehicleType = command.getVehicleType();
        Assert.assertTrue("The expected vehicle type, " + vehicleType + " could not be found in the row of command, " + commandName + ".", displayedText.contains(vehicleType));
        String speed = command.getSpeed().substring(0, 2);
        Assert.assertTrue("The expected speed of " + speed + " could not be found in the row of command, " + commandName + ".", displayedText.contains(speed));
        Assert.assertTrue("The sim time cell for the expected vehicle injection command, " + commandName + ", could not be found.", isTimeCellDisplayed(row));
        String displayedTime = getTimeCell(row).getText().substring(0, 2);
        Assert.assertEquals("The expected sim time injection value of " + simTime + " could not be found in the row of command, " + commandName + ".", simTime, displayedTime);
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(COMMAND_HEADER_CELL_XPATH));
    }

}
