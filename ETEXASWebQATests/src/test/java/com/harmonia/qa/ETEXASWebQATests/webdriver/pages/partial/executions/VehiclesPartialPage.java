package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * The partial page displayed as a part of the Execution Details page when the
 * Vehicles tab is selected
 *
 * @author llaroussini
 */
public class VehiclesPartialPage extends ExecutionPartialPage {

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public VehiclesPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Vehicle header cell titles
     *
     * @author llaroussini
     */
    public enum VehicleHeaderCells {
        /**
         * The Vehicle ID title for header cell
         */
        VEHCILE_ID("Vehicle ID"),
        /**
         * The Position title for header cell
         */
        POSITION("Position (x, y) (cm)"),
        /**
         * The Speed title for header cell
         */
        SPEED("Speed (m/s)"),
        /**
         * The Lane ID title for header cell
         */
        LANE_ID("Lane ID"),
        /**
         * The Distance title for header cell
         */
        DISTANCE("Distance to Intersection (cm)"),
        /**
         * The Time title for header cell
         */
        TIME("Time to Intersection (s)"),
        /**
         * The Signal State title for header cell
         */
        SIGNAL_STATE("Signal State");

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
        VehicleHeaderCells(String label) {
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
     * Xpath to the Vehicle table
     */
    private static final String VEHICLE_TABLE_XPATH = "//span[contains(@class, 'x-tab-inner-default')]/ancestor::node()[8]/div[@data-ref='body']/div/div[contains(@class, 'x-panel-body-default')]/div[1]/div[@class='x-grid-item-container']"; //TODO update xpath when unique identifiers are in place

    /**
     * Xpath prefix to vehicle column header cells
     */
    private static final String VEHICLE_COLUMN_HEADER_CELL_XPATH_PREFIX = "//span[contains(@class, 'x-tab-inner-default')]/ancestor::node()[8]/div//span[text()='"; //TODO update xpath when unique identifiers are in place

    /**
     * Xpath to the Position cell (used in conjunction with vehicle row to get
     * the specific cell)
     */
    private static final String POSITION_CELL_XPATH = ".//td[2]/div";

    /**
     * Xpath to the Speed cell (used in conjunction with vehicle row to get the
     * specific cell)
     */
    private static final String SPEED_CELL_XPATH = ".//td[3]/div";

    /**
     * Xpath to the Lane ID cell (used in conjunction with vehicle row to get
     * the specific cell)
     */
    private static final String LANE_ID_CELL_XPATH = ".//td[4]/div";

    /**
     * Xpath to the Distance to Intersection cell (used in conjunction with
     * vehicle row to get the specific cell)
     */
    private static final String DISTANCE_TO_INTERSECTION_CELL_XPATH = ".//td[5]/div";

    /**
     * Xpath to the Time to Intersection cell (used in conjunction with vehicle
     * row to get the specific cell)
     */
    private static final String TIME_TO_INTERSECTION_CELL_XPATH = ".//td[6]/div";

    /**
     * Xpath to the Signal State cell (used in conjunction with vehicle row to
     * get the specific cell)
     */
    private static final String SIGNAL_STATE_CELL_XPATH = ".//td[7]/div";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the vehicle table
     *
     * @return the vehicle table element
     */
    private El getVehicleTable() {
        return el(By.xpath(VEHICLE_TABLE_XPATH));
    }

    /**
     * Gets the given column header cell
     *
     * @param headerCell -the header cell to get
     * @return the cell element
     */
    private El getHeaderCell(VehicleHeaderCells headerCell) {
        return getVehicleTable().el(By.xpath(VEHICLE_COLUMN_HEADER_CELL_XPATH_PREFIX + headerCell.label + COLUMN_HEADER_CELL_XPATH_SUFFIX));
    }

    /**
     * Gets vehicle row based on given ID
     *
     * @param vehicleId -the vehicle ID expected
     * @return the row element
     */
    private El getVehicleRow(String vehicleId) {
        return rowInTable(vehicleId, getVehicleTable(), ".//table");
    }

    /**
     * Gets the Position cell for a given row
     *
     * @param vehicleRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getPositionCell(El vehicleRow) {
        return vehicleRow.el(By.xpath(POSITION_CELL_XPATH));
    }

    /**
     * Gets the Speed cell for a given row
     *
     * @param vehicleRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getSpeedCell(El vehicleRow) {
        return vehicleRow.el(By.xpath(SPEED_CELL_XPATH));
    }

    /**
     * Gets the Lane ID cell for a given row
     *
     * @param vehicleRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getLaneIDCell(El vehicleRow) {
        return vehicleRow.el(By.xpath(LANE_ID_CELL_XPATH));
    }

    /**
     * Gets the Distance to Intersection cell for a given row
     *
     * @param vehicleRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getDistanceToIntersectionCell(El vehicleRow) {
        return vehicleRow.el(By.xpath(DISTANCE_TO_INTERSECTION_CELL_XPATH));
    }

    /**
     * Gets the Time to Intersection cell for a given row
     *
     * @param vehicleRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getTimeToIntersectionCell(El vehicleRow) {
        return vehicleRow.el(By.xpath(TIME_TO_INTERSECTION_CELL_XPATH));
    }

    /**
     * Gets the Signal State cell for a given row
     *
     * @param vehicleRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getSignalStateCell(El vehicleRow) {
        return vehicleRow.el(By.xpath(SIGNAL_STATE_CELL_XPATH));
    }

    /**
     * Gets the position currently displayed for the given vehicle id
     *
     * @param vehicleID -the vehicle ID
     * @return the text displayed
     */
    public String getVehiclePosition(String vehicleId) {
        El vehicleRow = getVehicleRow(vehicleId);
        return getPositionCell(vehicleRow).getText();
    }

    /**
     * Gets the speed currently displayed for the given vehicle id
     *
     * @param vehicleID -the vehicle ID
     * @return the text displayed
     */
    public String getVehicleSpeed(String vehicleId) {
        El vehicleRow = getVehicleRow(vehicleId);
        return getSpeedCell(vehicleRow).getText();
    }

    /**
     * Gets the lane ID currently displayed for the given vehicle id
     *
     * @param vehicleID -the vehicle ID
     * @return the text displayed
     */
    public String getVehicleLaneID(String vehicleId) {
        El vehicleRow = getVehicleRow(vehicleId);
        return getLaneIDCell(vehicleRow).getText();
    }

    /**
     * Gets the distance to intersection currently displayed for the given
     * vehicle id
     *
     * @param vehicleID -the vehicle ID
     * @return the text displayed
     */
    public String getVehicleDistanceToIntersection(String vehicleId) {
        El vehicleRow = getVehicleRow(vehicleId);
        return getDistanceToIntersectionCell(vehicleRow).getText();
    }

    /**
     * Gets the time to intersection currently displayed for the given vehicle
     * id
     *
     * @param vehicleID -the vehicle ID
     * @return the text displayed
     */
    public String getVehicleTimeToIntersectionPosition(String vehicleId) {
        El vehicleRow = getVehicleRow(vehicleId);
        return getTimeToIntersectionCell(vehicleRow).getText();
    }

    /**
     * Gets the signal state currently displayed for the given vehicle id
     *
     * @param vehicleID -the vehicle ID
     * @return the text displayed
     */
    public String getSignalState(String vehicleId) {
        El vehicleRow = getVehicleRow(vehicleId);
        return getSignalStateCell(vehicleRow).getText();
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
    public boolean isHeaderCellDisplayed(VehicleHeaderCells headerCell) {
        return isElementDisplayed(getHeaderCell(headerCell));
    }

    /**
     * Checks to see if vehicle rows are displayed
     *
     * @return true if displayed, false if not
     */
    public boolean areVehicleRowsDisplayed() {
        try {
            return !(getRows(getVehicleTable()).isEmpty());
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if row with given vehicle Id is displayed
     *
     * @param vehicleId -the vehicle ID expected
     * @return true if displayed, false if not found or not visible
     */
    public boolean isVehicleRowDisplayed(String vehicleId) {
        return isElementDisplayed(getVehicleRow(vehicleId));
    }

    /**
     * Checks if text is present in the speed cell for the given vehicle row
     *
     * @param vehicleId -the vehicle id to check
     * @return true if text is present, false if no text
     */
    public boolean isSpeedTextDisplayed(String vehicleId) {
        String speed = getVehicleSpeed(vehicleId);
        return !speed.isEmpty();
    }

    /**
     * Checks if text is present in the lane ID cell for the given vehicle row
     *
     * @param vehicleId -the vehicle id to check
     * @return true if text is present, false if no text
     */
    public boolean isLaneIDTextDisplayed(String vehicleId) {
        String laneID = getVehicleLaneID(vehicleId);
        return !laneID.isEmpty();
    }

    /**
     * Checks if text is present in the distance to intersection cell for the
     * given vehicle row
     *
     * @param vehicleId -the vehicle id to check
     * @return true if text is present, false if no text
     */
    public boolean isDistanceToIntersectionTextDisplayed(String vehicleId) {
        String distance = getVehicleDistanceToIntersection(vehicleId);
        return !distance.isEmpty();
    }

    /**
     * Checks if text is present in the time to intersection cell for the given
     * vehicle row
     *
     * @param vehicleId -the vehicle id to check
     * @return true if text is present, false if no text
     */
    public boolean isTimeToIntersectionTextDisplayed(String vehicleId) {
        String time = getVehicleTimeToIntersectionPosition(vehicleId);
        return !time.isEmpty();
    }

    /**
     * Checks if text is present in the signal state cell for the given vehicle
     * row
     *
     * @param vehicleId -the vehicle id to check
     * @return true if text is present, false if no text
     */
    public boolean isSignalStateTextDisplayed(String vehicleId) {
        String signalState = getSignalState(vehicleId);
        return !signalState.isEmpty();
    }

    /**
     * Checks if text is present in the position cell for the given vehicle row
     *
     * @param vehicleRow -the vehicle row to check
     * @return true if text is present, false if no text
     */
    private boolean isPostionTextDisplayed(El vehicleRow) {
        String position = getPositionCell(vehicleRow).getText();
        return !position.isEmpty();
    }

    /**
     * Checks if info cell is displayed with given info in given vehicle row
     *
     * @param vehicleRow -the row where info is expected
     * @param info -the info expected
     * @return true if displayed, false if not
     */
    private boolean isVehicleInfoCellDisplayed(El vehicleRow, String info) {
        return isElementDisplayed(getInfoCell(vehicleRow, info));
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Checks for presence of all column header cells (Vehicle ID, Position,
     * Speed, Lane ID, Distance, Time, Signal State)
     */
    public void checkAllColumnHeaderCells() {
        Assert.assertTrue("Vehicle ID column header cell could not be found or is not visible.", isHeaderCellDisplayed(VehicleHeaderCells.VEHCILE_ID));
        Assert.assertTrue("Position column header cell could not be found or is not visible.", isHeaderCellDisplayed(VehicleHeaderCells.POSITION));
        Assert.assertTrue("Speed column header cell could not be found or is not visible.", isHeaderCellDisplayed(VehicleHeaderCells.SPEED));
        Assert.assertTrue("Lane ID column header cell could not be found or is not visible.", isHeaderCellDisplayed(VehicleHeaderCells.LANE_ID));
        Assert.assertTrue("Distance column header cell could not be found or is not visible.", isHeaderCellDisplayed(VehicleHeaderCells.DISTANCE));
        Assert.assertTrue("Time column header cell could not be found or is not visible.", isHeaderCellDisplayed(VehicleHeaderCells.TIME));
        Assert.assertTrue("Signal State column header cell could not be found or is not visible.", isHeaderCellDisplayed(VehicleHeaderCells.SIGNAL_STATE));

    }

    /**
     * Verifies that values are not null in all columns (unable to check for
     * specific values as values are dynamic in each execution)
     *
     * @param vehicleID -the vehicle ID
     */
    public void checkVehicleInfo(String vehicleID) {
        El row = getVehicleRow(vehicleID);
        Assert.assertTrue("No text was found in the vehicle position cell.", isPostionTextDisplayed(row));
        Assert.assertTrue("No text was found in the vehicle speed cell.", isSpeedTextDisplayed(vehicleID));
        Assert.assertTrue("No text was found in the vehicle lane ID cell.", isLaneIDTextDisplayed(vehicleID));
        Assert.assertTrue("No text was found in the vehicle's distance to intersection cell.", isDistanceToIntersectionTextDisplayed(vehicleID));
        Assert.assertTrue("No text was found in the vehicle's time to intersection cell.", isTimeToIntersectionTextDisplayed(vehicleID));
        Assert.assertTrue("No text was found in the vehicle's signal state cell.", isSignalStateTextDisplayed(vehicleID));

    }

    /**
     * Gets the number of vehicle rows currently displayed
     *
     * @return the number of rows displayed
     */
    public String getVehicleRows() {
        int rows = getRows(getVehicleTable()).size();
        return Integer.toString(rows);
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(VEHICLE_TABLE_XPATH));
    }

}
