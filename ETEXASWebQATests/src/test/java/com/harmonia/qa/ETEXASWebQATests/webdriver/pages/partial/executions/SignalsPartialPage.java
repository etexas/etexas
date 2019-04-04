package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * The partial page displayed as a part of the Execution Details page when the
 * Signals tab is selected
 *
 * @author llaroussini
 */
public class SignalsPartialPage extends ExecutionPartialPage {

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public SignalsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Signal header cell titles
     *
     * @author llaroussini
     */
    public enum SignalHeaderCells {
        /**
         * The Lane ID title for header cell
         */
        LANE_ID("Lane ID"),
        /**
         * The Signal Type title for header cell
         */
        SIGNAL_TYPE("Signal Type"),
        /**
         * The State title for header cell
         */
        STATE("State"),
        /**
         * The Color title for header cell
         */
        COLOR("Color"),
        /**
         * The Time to Change title for header cell
         */
        TIME_TO_CHANGE("Time to Change (s)");

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
        SignalHeaderCells(String label) {
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
     * Xpath to the Signals table
     */
    private static final String SIGNAL_TABLE_XPATH = "//span[contains(@class, 'x-tab-inner-default')]/ancestor::node()[8]/div[@data-ref='body']/node()[2]/node()[2]/div/div[@class='x-grid-item-container']"; //TODO update xpath when unique identifiers are in place

    /**
     * Xpath prefix to signal column header cells
     */
    private static final String SIGNAL_COLUMN_HEADER_CELL_XPATH_PREFIX = "//span[contains(@class, 'x-tab-inner-default')]/ancestor::node()[8]/div/div[2]//span[text()='"; //TODO update xpath when unique identifiers are in place

    /**
     * Xpath to the Lane ID header cell
     */
    private static final String LANE_ID_SIGNAL_HEADER_CELL_XPATH = SIGNAL_COLUMN_HEADER_CELL_XPATH_PREFIX + SignalHeaderCells.LANE_ID.label + COLUMN_HEADER_CELL_XPATH_SUFFIX;

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the signals table
     *
     * @return the signal table element
     */
    private El getSignalsTable() {
        return el(By.xpath(SIGNAL_TABLE_XPATH));
    }

    /**
     * Gets the given column header cell
     *
     * @param headerCell -the header cell to get
     * @return the cell element
     */
    private El getHeaderCell(SignalHeaderCells headerCell) {
        return getSignalsTable().el(By.xpath(SIGNAL_COLUMN_HEADER_CELL_XPATH_PREFIX + headerCell.label + COLUMN_HEADER_CELL_XPATH_SUFFIX));
    }

    /**
     * Gets signal row based on given lane ID
     *
     * @param laneID -the lane ID expected to be present in signals table
     * @return the row element
     */
    private El getSignalRow(String laneID) {
        return rowInTable(laneID, getSignalsTable(), ".//tr");
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
    public boolean isHeaderCellDisplayed(SignalHeaderCells headerCell) {
        return isElementDisplayed(getHeaderCell(headerCell));
    }

    /**
     * Checks to see if signal rows are displayed
     *
     * @return true if displayed, false if not
     */
    public boolean areSignalRowsDisplayed() {
        try {
            return !(getRows(getSignalsTable()).isEmpty());
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if row with given lane Id is displayed in signals table
     *
     * @param laneID -the lane ID of the signal expected
     * @return true if displayed, false if not found or not visible
     */
    public boolean isSignalRowDisplayed(String laneID) {
        return isElementDisplayed(getSignalRow(laneID));
    }

    /**
     * Checks if info cell is displayed with given info in given signal row
     *
     * @param signalRow -the row where info is expected
     * @param info -the info expected
     * @return true if displayed, false if not
     */
    private boolean isSignalInfoCellDisplayed(El signalRow, String info) {
        return isElementDisplayed(getInfoCell(signalRow, info));
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Checks for presence of all column header cells (Lane ID, Signal Type,
     * State, Color, Time to Change)
     */
    public void checkAllColumnHeaderCells() {
        Assert.assertTrue("Lane ID column header cell could not be found or is not visible.", isHeaderCellDisplayed(SignalHeaderCells.LANE_ID));
        Assert.assertTrue("Signal Type column header cell could not be found or is not visible.", isHeaderCellDisplayed(SignalHeaderCells.SIGNAL_TYPE));
        Assert.assertTrue("State column header cell could not be found or is not visible.", isHeaderCellDisplayed(SignalHeaderCells.STATE));
        Assert.assertTrue("Color column header cell could not be found or is not visible.", isHeaderCellDisplayed(SignalHeaderCells.COLOR));
        Assert.assertTrue("Time to Change column header cell could not be found or is not visible.", isHeaderCellDisplayed(SignalHeaderCells.TIME_TO_CHANGE));

    }

    /**
     * Checks known values for a signal row and asserts they can be found as
     * displayed in the row (row is found by given lane)
     *
     * @param lane -the lane of the signal
     * @param state -the current state of the signal
     * @param color -the current color of the signal
     * @param timeToChange -the current time it will take for the signal to
     *        change
     */
    public void checkSignalInfo(Lane lane, String state, String color, String timeToChange) {
        String laneID = lane.getLaneID().getLabel();
        String signalType = lane.getSignal().getSignalType().getLabel();
        El row = getSignalRow(laneID);
        Assert.assertTrue("Signal type, " + signalType + ", could not be found in the row with Lane ID of " + laneID + ".", isSignalInfoCellDisplayed(row, signalType));
        Assert.assertTrue("Signal state, " + state + ", could not be found in the row with Lane ID of " + laneID + ".", isSignalInfoCellDisplayed(row, state));
        Assert.assertTrue("Signal color, " + color + ", could not be found in the row with Lane ID of " + laneID + ".", isSignalInfoCellDisplayed(row, color));
        Assert.assertTrue("Time to change of " + timeToChange + " could not be found in the row with Lane ID of " + laneID + ".", isSignalInfoCellDisplayed(row, timeToChange));
    }

    /**
     * Checks for presence of all signal rows for the given simulation
     *
     * @param sim -the simulation
     */
    public void checkSignalRowsDisplayed(TemplateSimulation sim) {
        List<Lane> lanes = sim.getLanes();
        for (Lane lane : lanes) {
            String laneID = lane.getLaneID().getLabel();
            El row = getSignalRow(laneID);
            Assert.assertTrue("Lane ID, " + laneID + ", could not be found.", isSignalInfoCellDisplayed(row, laneID));
        }
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(LANE_ID_SIGNAL_HEADER_CELL_XPATH));
    }

}
