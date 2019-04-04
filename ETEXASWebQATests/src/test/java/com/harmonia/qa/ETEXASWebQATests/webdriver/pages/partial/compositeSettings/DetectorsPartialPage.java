package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.ExecutionPartialPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * The partial page displayed as a part of the Execution Details page when the
 * Detectors tab is selected
 *
 * @author llaroussini
 */
public class DetectorsPartialPage extends ExecutionPartialPage {

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public DetectorsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Detector header cell titles
     *
     * @author llaroussini
     */
    public enum DetectorHeaderCells {
        /**
         * The ID title for header cell
         */
        ID("ID"),
        /**
         * The Lanes title for header cell
         */
        LANES("Lanes"),
        /**
         * The Event title for header cell
         */
        EVENT("Event"),
        /**
         * The Detector Type title for header cell
         */
        DETECTOR_TYPE("Detector Type"),
        /**
         * The Boundary Points title for header cell
         */
        BOUNDARY_POINTS("Boundary Points (x, y) (cm)");

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
        DetectorHeaderCells(String label) {
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
     * Xpath to the Detectors table
     */
    private static final String DETECTORS_TABLE_XPATH = "//span[contains(@class, 'x-tab-inner-default')][text()='Signals']/ancestor::node()[8]/div[@data-ref='body']/node()[3]/node()[2]/div/div[@class='x-grid-item-container']"; //TODO update xpath when unique identifiers are in place

    /**
     * Xpath prefix to detectors column header cells
     */
    private static final String DETECTORS_COLUMN_HEADER_CELL_XPATH_PREFIX = "//span[contains(@class, 'x-tab-inner-default')]/ancestor::node()[8]/div/div[3]//span[text()='"; //TODO update xpath when unique identifiers are in place

    /**
     * Xpath to the Detector ID cell (used in conjunction with detector row to
     * get the specific cell)
     */
    private static final String DETECTOR_ID_CELL_XPATH = ".//td[2]/div";

    /**
     * Xpath to the Event cell (used in conjunction with detector row to get the
     * specific cell)
     */
    private static final String EVENT_CELL_XPATH = ".//td[4]/div";

    /**
     * Xpath to the Detector Type cell (used in conjunction with detector row to
     * get the specific cell)
     */
    private static final String DETECTOR_TYPE_CELL_XPATH = ".//td[5]/div";

    /**
     * Xpath to the Boundary Points cell (used in conjunction with detector row
     * to get the specific cell)
     */
    private static final String BOUNDARY_POINTS_CELL_XPATH = ".//td[6]/div";

    /**
     * Xpath to the Detector Type header cell
     */
    private static final String DETECTOR_TYPE_HEADER_CELL_XPATH = DETECTORS_COLUMN_HEADER_CELL_XPATH_PREFIX + DetectorHeaderCells.DETECTOR_TYPE.label + COLUMN_HEADER_CELL_XPATH_SUFFIX;

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the detectors table
     *
     * @return the detectors table element
     */
    private El getDetectorsTable() {
        return el(By.xpath(DETECTORS_TABLE_XPATH));
    }

    /**
     * Gets the given column header cell
     *
     * @param headerCell -the header cell to get
     * @return the cell element
     */
    private El getHeaderCell(DetectorHeaderCells headerCell) {
        return getDetectorsTable().el(By.xpath(DETECTORS_COLUMN_HEADER_CELL_XPATH_PREFIX + headerCell.label + COLUMN_HEADER_CELL_XPATH_SUFFIX));
    }

    /**
     * Gets detector row based on given detector (based on the lane associated
     * with the detector)
     *
     * @param detector -the detector expected
     * @return the row element
     */
    private El getDetectorRow(Detector detector) {
        String laneID = detector.getLane().getLaneID().getLabel();
        return rowInTable(laneID, getDetectorsTable(), ".//table");
    }

    /**
     * Gets the detector ID cell for a given row
     *
     * @param detectorRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getDetectorIDCell(El detectorRow) {
        return detectorRow.el(By.xpath(DETECTOR_ID_CELL_XPATH));
    }

    /**
     * Gets the event cell for a given row
     *
     * @param detectorRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getEventCell(El detectorRow) {
        return detectorRow.el(By.xpath(EVENT_CELL_XPATH));
    }

    /**
     * Gets the detector type cell for a given row
     *
     * @param detectorRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getDetectorTypeCell(El detectorRow) {
        return detectorRow.el(By.xpath(DETECTOR_TYPE_CELL_XPATH));
    }

    /**
     * Gets the boundary points cell for a given row
     *
     * @param detectorRow -the row in which the cell is needed
     * @return the cell element
     */
    private El getBoundaryPointsCell(El detectorRow) {
        return detectorRow.el(By.xpath(BOUNDARY_POINTS_CELL_XPATH));
    }

    /**
     * Gets the detector ID currently displayed for the given detector
     *
     * @param detector -the detector
     * @return the text displayed
     */
    public String getDetectorID(Detector detector) {
        El detectorRow = getDetectorRow(detector);
        return getDetectorIDCell(detectorRow).getText();
    }

    /**
     * Gets the event currently displayed for the given detector
     *
     * @param detector -the detector
     * @return the text displayed
     */
    public String getEvent(Detector detector) {
        El detectorRow = getDetectorRow(detector);
        return getEventCell(detectorRow).getText();
    }

    /**
     * Gets the detector type currently displayed for the given detector
     *
     * @param detector -the detector
     * @return the text displayed
     */
    public String getDetectorType(Detector detector) {
        El detectorRow = getDetectorRow(detector);
        return getDetectorTypeCell(detectorRow).getText();
    }

    /**
     * Gets the boundary points currently displayed for the given detector
     *
     * @param detector -the detector
     * @return the text displayed
     */
    public String getBoundaryPoints(Detector detector) {
        El detectorRow = getDetectorRow(detector);
        return getBoundaryPointsCell(detectorRow).getText();
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
    public boolean isHeaderCellDisplayed(DetectorHeaderCells headerCell) {
        return isElementDisplayed(getHeaderCell(headerCell));
    }

    /**
     * Checks to see if detector rows are displayed
     *
     * @return true if displayed, false if not
     */
    public boolean areDetectorRowsDisplayed() {
        try {
            return !(getRows(getDetectorsTable()).isEmpty());
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if row with given detector is displayed (based on the lane in
     * which the detector exists)
     *
     * @param detector -the detector expected
     * @return true if displayed, false if not found or not visible
     */
    public boolean isDetectorRowDisplayed(Detector detector) {
        return isElementDisplayed(getDetectorRow(detector));
    }

    /**
     * Checks if text is present in the ID cell for the given detector
     *
     * @param detector -the detector to check
     * @return true if text is present, false if no text
     */
    private boolean isDetectorIDTextDisplayed(Detector detector) {
        String detectorID = getDetectorID(detector);
        return !detectorID.isEmpty();
    }

    /**
     * Checks if text is present in the detector type cell for the given
     * detector
     *
     * @param detector -the detector to check
     * @return true if text is present, false if no text
     */
    private boolean isDetectorTypeTextDisplayed(Detector detector) {
        String type = getDetectorType(detector);
        return !type.isEmpty();
    }

    /**
     * Checks if text is present in the event cell for the given detector
     *
     * @param detector -the detector to check
     * @return true if text is present, false if no text
     */
    private boolean isEventTextDisplayed(Detector detector) {
        String event = getEvent(detector);
        return !event.isEmpty();
    }

    /**
     * Checks if text is present in the boundary points cell for the given
     * detector
     *
     * @param detector -the detector to check
     * @return true if text is present, false if no text
     */
    private boolean isBoundaryPointsTextDisplayed(Detector detector) {
        String boundary = getBoundaryPoints(detector);
        return !boundary.isEmpty();
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Checks for presence of all column header cells (ID, Lanes, Event,
     * Detector Type, Boundary Points)
     */
    public void checkAllColumnHeaderCells() {
        Assert.assertTrue("ID column header cell could not be found or is not visible.", isHeaderCellDisplayed(DetectorHeaderCells.ID));
        Assert.assertTrue("Lanes Type column header cell could not be found or is not visible.", isHeaderCellDisplayed(DetectorHeaderCells.LANES));
        Assert.assertTrue("Event column header cell could not be found or is not visible.", isHeaderCellDisplayed(DetectorHeaderCells.EVENT));
        Assert.assertTrue("Detector Type column header cell could not be found or is not visible.", isHeaderCellDisplayed(DetectorHeaderCells.DETECTOR_TYPE));
        Assert.assertTrue("Boundary Points to Change column header cell could not be found or is not visible.", isHeaderCellDisplayed(DetectorHeaderCells.BOUNDARY_POINTS));
    }

    /**
     * Verifies that values are not null in all columns (unable to check for
     * specific values as values are dynamic in each execution)
     *
     * @param detector -the detector expected
     */
    public void checkDetectorInfo(Detector detector) {
        String laneID = detector.getLane().getLaneID().getLabel();
        Assert.assertTrue("Detector associated with lane ID, " + laneID + ", could not be found.", isDetectorRowDisplayed(detector));
        Assert.assertTrue("No text was found in the ID cell for detector associated with lane ID, " + laneID + ".", isDetectorIDTextDisplayed(detector));
        Assert.assertTrue("No text was found in the event cell for detector associated with lane ID, " + laneID + ".", isEventTextDisplayed(detector));
        Assert.assertTrue("No text was found in the type cell for detector associated with lane ID, " + laneID + ".", isDetectorTypeTextDisplayed(detector));
        Assert.assertTrue("No text was found in the detector boundary points cell for detector associated with lane ID, " + laneID + ".", isBoundaryPointsTextDisplayed(detector));

    }
    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(DETECTOR_TYPE_HEADER_CELL_XPATH));
    }

}
