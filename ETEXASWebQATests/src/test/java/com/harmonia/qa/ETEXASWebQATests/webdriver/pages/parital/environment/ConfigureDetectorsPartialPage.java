package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.CreateDetectorModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal.ConfigureBtn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.EditDetectorModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configuration of detectors partial page
 *
 * @author rsmith
 */
public class ConfigureDetectorsPartialPage extends SimulationSettingsModal {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public ConfigureDetectorsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////
    // Enumerations
    ///////////////

    /**
     * Enumeration of columns in detectors table
     *
     * @author rsmith
     */
    public enum DetectorsTableColumnHeader {
        /**
         * ID column header
         */
        ID("ID"),

        /**
         * Lane column header
         */
        LANE("Lane"),

        /**
         * Width column header
         */
        WIDTH("Width (cm)"),

        /**
         * Height column header
         */
        HEIGHT("Height (cm)"),

        /**
         * Distance column header
         */
        DISTANCE("Distance from Stop Line (cm)");

        /**
         * The label of the column header as appears in the application
         */

        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        DetectorsTableColumnHeader(String label) {
            this.label = label;
        }

        /**
         * Get's the label of the column header
         *
         * @return the label of the column header
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Xpath prefix to detectors table column
     */
    private static final String DETECTORS_TABLE_COLUMN_XPATH_PREFIX = "//div[contains(@id, 'detector')]//span[@class='x-column-header-text-inner'][text()='";

    /**
     * Xpath prefix to buttons
     */
    private static final String CONFIGURE_DETECTOR_BTN_XPATH_PREFIX = "//div[contains(@id, 'detector')]//span[contains(@id, 'button')][text()='";

    /**
     * Xpath to the ID cell contained in a row
     */
    private static final String ID_DETECTOR_XPATH = "./td[contains(@class, 'x-grid-cell-first')]//div";

    /**
     * Xpath to the detectors table
     */
    private static final String DETECTOR_TABLE_XPATH = "//div[contains(@id, 'detector')]//table";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the given column header element
     *
     * @param column -the column to get
     * @return the column header element
     */
    private El getDetectorColumnHeader(DetectorsTableColumnHeader column) {
        return el(By.xpath(DETECTORS_TABLE_COLUMN_XPATH_PREFIX + column.getLabel() + "']"));
    }

    /**
     * Gets the given button
     *
     * @param btn -the button to get
     * @return the button
     */
    private El getDetectorBtn(ConfigureBtn btn) {
        return el(By.xpath(CONFIGURE_DETECTOR_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the row associated with the given detector
     *
     * @param detector -the cell associated with the detector
     * @return the row element
     */
    private El getDetectorRow(String uniqueValue) {
        El detectorCell = el(getCell(uniqueValue));
        return detectorCell.el(By.xpath("./ancestor::tr"));
    }

    /**
     * Gets the ID associated with the given detector
     *
     * @param Detector -the detector
     * @return the ID as string
     */
    public String getDetectorID(Detector detector) {
        El row = getDetectorRow(detector.getHeight());
        El idDetector = row.el(By.xpath(ID_DETECTOR_XPATH));
        return idDetector.getText();
    }

    /**
     * Gets the Detectors table
     *
     * @return the table element
     */
    private El getDetectorTable() {
        return el(By.xpath(DETECTOR_TABLE_XPATH));
    }

    /**
     * Checks to see if detector row is displayed
     *
     * @param detector -the detector expected
     * @return true if displayed, false otherwise
     */
    public boolean isDetectorRowDisplayed(Detector detector) {
        El row = getDetectorRow(detector);
        if (row != null) {
            return isElementDisplayed(row);
        }
        else {
            return false;
        }
    }

    /**
     * Gets the Detector Row and values within the row. Iterates the rows to
     * verify if the text equals the added values to the detector below, then it
     * will return the row.
     *
     * @param detector to check
     * @return the values for the detector (width,height,distance)
     */
    private El getDetectorRow(Detector detector) {
        List<String> detectorValues = new ArrayList<String>(6);
        detectorValues.add(detector.getID());
        detectorValues.add(detector.getLane().getLaneID().getLabel());
        detectorValues.add(detector.getWidth());
        detectorValues.add(detector.getHeight());
        detectorValues.add(detector.getDistance());
        List<El> rows = getDetectorTable().els(By.xpath(".//tr"));
        El theRow = null;

        Iterator<El> iterator = rows.iterator();
        while (iterator.hasNext() && theRow == null) {
            El row = iterator.next();

            List<El> cells = row.els(By.xpath(".//td//div"));
            List<String> textList = new ArrayList<String>();
            for (El cell : cells) {
                String cellValue = cell.getText();
                textList.add(cellValue);
            }
            if (textList.equals(detectorValues)) {
                theRow = row;
            }

        }

        return theRow;
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the column header expected is displayed
     *
     * @param column -the column expected
     * @return true if displayed, false otherwise
     */
    public boolean isDetectorColumnHeaderDisplayed(DetectorsTableColumnHeader column) {
        return isElementDisplayed(getDetectorColumnHeader(column));
    }

    /**
     * Checks to see if the detector create button is displayed
     *
     * @return true if detector create button is displayed, false if it is not
     *         or cannot be found
     */
    public boolean isDetectorCreateBtnDisplayed1() {
        return isElementDisplayed(getDetectorBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the detector edit button is displayed
     *
     * @return true if the detector edit button is displayed, false if it is not
     *         or cannot be found
     */
    public boolean isDetectorEditBtnDisplayed() {
        return isElementDisplayed(getDetectorBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks to see if the detector delete button is displayed
     *
     * @return true if the detector delete button is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isDetectorDeleteBtnDisplayed() {
        return isElementDisplayed(getDetectorBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the detector create button is enabled
     *
     * @return true if the detector create button is enabled, false if it is
     *         disabled
     */
    public boolean isDetectorCreateBtnEnabled() {
        return getDetectorBtn(ConfigureBtn.CREATE).isEnabled();
    }

    /**
     * Checks to see if the detector edit button is enabled
     *
     * @return true if the detector edit button is enabled, false if it is
     *         disabled
     */
    public boolean isDetectorEditBtnEnabled() {
        return getDetectorBtn(ConfigureBtn.EDIT).isEnabled();
    }

    /**
     * Checks to see if the detector delete button is enabled
     *
     * @return true if the detector delete button is enabled, false if it is
     *         disabled
     */
    public boolean isDetectorDeleteBtnEnabled() {
        return getDetectorBtn(ConfigureBtn.DELETE).isEnabled();
    }

    /**
     * Checks to see if the given detector attribute is displayed in the table
     *
     * @param attirbute -the attribute associated with the detector
     * @return true if displayed, false if not
     */
    public boolean isDetectorDisplayed(String attibute) {
        return isElementDisplayed(el(getCell(attibute)));
    }

    /**
     * Checks if the row for the given detector is selected
     *
     * @param detector to check
     * @return true if selected, false if deselected
     */
    public boolean isDetectorRowSelected(Detector detector) {
        String id = detector.getID();
        return isRowSelected(id);
    }

    /**
     * Checks if there are detectors displayed
     *
     * @return true if displayed
     */
    public boolean isDetectorTableDisplayed() {
        return isElementDisplayed(getDetectorTable());
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Clicks the Create button
     *
     * @return the newly loaded detector Form
     */
    public CreateDetectorModal clickCreateDetectorBtn() {
        getDetectorBtn(ConfigureBtn.CREATE).click();
        return getPage(CreateDetectorModal.class);
    }

    /**
     * Clicks the Delete button
     *
     * @return the Delete Warning Form
     */
    public ConfirmDeleteModal clickDeleteDetectorBtn() {
        getDetectorBtn(ConfigureBtn.DELETE).click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the newly loaded Edit Cell Tower form
     */
    public EditDetectorModal clickEdit() {
        getDetectorBtn(ConfigureBtn.EDIT).click();
        return getPage(EditDetectorModal.class);
    }

    //////////////
    // Utilities
    /////////////

    /**
     * Verifies all expected buttons are displayed (Create, Edit, and Delete)
     */
    public void checkBtns() {
        Assert.assertTrue("Create button not displayed as expected.", isDetectorCreateBtnDisplayed1());
        Assert.assertTrue("Edit button not displayed as expected.", isDetectorEditBtnDisplayed());
        Assert.assertTrue("Delete button not displayed as expected.", isDetectorDeleteBtnDisplayed());
    }

    /**
     * Verifies all column headers are displayed in Detectors table (ID, Lane,
     * Width, Height, and Distance from Stop Line)
     */
    public void checkDetectorColumnHeaders() {
        Assert.assertTrue("The ID column header not displayed as expected in Detectors table.", isDetectorColumnHeaderDisplayed(DetectorsTableColumnHeader.ID));
        Assert.assertTrue("Lane Number header cell not displayed as expected.", isDetectorColumnHeaderDisplayed(DetectorsTableColumnHeader.LANE));
        Assert.assertTrue("Width header cell not displayed as expected.", isDetectorColumnHeaderDisplayed(DetectorsTableColumnHeader.WIDTH));
        Assert.assertTrue("Height header cell not displayed as expected.", isDetectorColumnHeaderDisplayed(DetectorsTableColumnHeader.HEIGHT));
        Assert.assertTrue("Distance from Stop Line header cell not displayed as expected.", isDetectorColumnHeaderDisplayed(DetectorsTableColumnHeader.DISTANCE));
    }

}