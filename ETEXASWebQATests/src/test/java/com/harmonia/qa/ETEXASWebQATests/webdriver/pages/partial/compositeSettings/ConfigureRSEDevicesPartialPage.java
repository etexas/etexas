package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal.ConfigureBtn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configuration of RSE devices partial page
 *
 * @author llaroussini
 */
public class ConfigureRSEDevicesPartialPage extends SimulationSettingsModal {

    /**
     * Default Constructor
     *
     * @param driver -the web driver
     */
    public ConfigureRSEDevicesPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////
    //Enumerations
    //////////////

    /**
     * Enumeration of columns in RSE devices table
     *
     * @author llaroussini
     */
    public enum RSEDeviceTableColumnHeader {
        /**
         * ID column header
         */
        ID("ID"),
        /**
         * Name column header
         */
        NAME("Name"),
        /**
         * X Coordinate column header
         */
        X_COORDINATE("X (cm)"),
        /**
         * Y Coordinate column header
         */
        Y_COORDINATE("Y (cm)"),
        /**
         * Z Coordinate column header
         */
        Z_COORDINATE("Z (cm)");

        /**
         * The label of the column header as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        RSEDeviceTableColumnHeader(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the column header as it is displayed
         * in the Web UI
         *
         * @return The label of the column header
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Xpath prefix to RSE table column
     */
    private static final String RSE_TABLE_COLUMN_XPATH_PREFIX = "//div[contains(@id, 'rse')][contains(@id, 'Settings')]//span[@class='x-column-header-text-inner'][text()='";

    /**
     * Xpath of the RSE table
     */
    private static final String RSE_TABLE_XPATH = "//div[contains(@id, 'rse')][contains(@class, 'x-grid-with-row-lines')]";

    /**
     * Xpath prefix to buttons
     */
    private static final String CONFIGURE_RSE_BTN_XPATH_PREFIX = "//div[contains(@id, 'rse')][contains(@id, 'Settings')]//span[contains(@id, 'btnInnerEl')][text()='";

    /**
     * Xpath suffix to buttons
     */
    private static final String CONFIGURE_RSE_BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to the ID cell contained in a row
     */
    private static final String ID_CELL_XPATH = "./td[contains(@class, 'x-grid-cell-first')]//div";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the given column header element
     *
     * @param column -the column to get
     * @return the column header element
     */
    private El getRSEColumnHeader(RSEDeviceTableColumnHeader column) {
        return el(By.xpath(RSE_TABLE_COLUMN_XPATH_PREFIX + column.label + "']"));
    }

    /**
     * Gets the RSE Devices table
     *
     * @return the table element
     */
    private El getRSETable() {
        return el(By.xpath(RSE_TABLE_XPATH));
    }

    /**
     * Gets the given button
     *
     * @param btn -the button to get
     * @return the button
     */
    private El getRSEBtn(ConfigureBtn btn) {
        return el(By.xpath(CONFIGURE_RSE_BTN_XPATH_PREFIX + btn.getLabel() + CONFIGURE_RSE_BTN_XPATH_SUFFIX));
    }

    /**
     * Gets the cell containing the given RSE name
     *
     * @param rseName -the name of the RSE
     * @return the cell containing the given RSE name
     */
    private El getRSENameCell(String rseName) {
        return el(getCell(rseName));
    }

    /**
     * Gets the row containing the given RSE Device name
     *
     * @param rseName -the name of the RSE device
     * @return the row containing the given name
     */
    private El getRSERowByName(String rseName) {
        El cell = getRSENameCell(rseName);
        return cell.el(By.xpath("./ancestor::tr"));
    }

    /**
     * Gets the RSE Row and values within the row. Iterates the rows to verify
     * if the text equals the added values to the RSE below, then it will return
     * the row.
     *
     * @param rse -the RSE device to check
     * @return the values for the RSE (ID, name, x coordinate, y coordinate, z
     *         coordinate)
     */
    private El getRSERow(RSEDevice rse) {
        waitForElementToBeVisible(By.xpath(RSE_TABLE_XPATH));
        List<String> rseValues = new ArrayList<String>(6);
        //Order of value addition must match order of columns in RSE table to ensure lists will match when values are obtained from UI
        rseValues.add(rse.getID());
        rseValues.add(rse.getName());
        rseValues.add(rse.getXCoordinate());
        rseValues.add(rse.getYCoordinate());
        rseValues.add(rse.getZCoordinate());
        List<El> rows = getRSETable().els(By.xpath(".//table//tr"));
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
            if (textList.equals(rseValues)) {
                theRow = row;
            }

        }

        return theRow;
    }

    /**
     * Gets the ID associated with the given RSE device
     *
     * @param rse -the RSE device
     * @return the ID as string
     */
    public String getRSEID(RSEDevice rse) {
        El row = getRSERowByName(rse.getName());
        El idCell = row.el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
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
    public boolean isRSEColumnHeaderDisplayed(RSEDeviceTableColumnHeader column) {
        return isElementDisplayed(getRSEColumnHeader(column));
    }

    /**
     * Checks if RSE table is displayed (only displays if devices are present)
     *
     * @return true if displayed, false otherwise
     */
    public boolean areRSEDevicesDisplayed() {
        return isElementDisplayed(getRSETable());
    }

    /**
     * Checks to see if the RSE create button is displayed
     *
     * @return true if create button is displayed, false otherwise
     */
    public boolean isRSECreateBtnDisplayed() {
        return isElementDisplayed(getRSEBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the RSE edit button is displayed
     *
     * @return true if the edit button is displayed, false otherwise
     */
    public boolean isRSEEditBtnDisplayed() {
        return isElementDisplayed(getRSEBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks to see if the RSE delete button is displayed
     *
     * @return true if the delete button is displayed, false otherwise
     */
    public boolean isRSEDeleteBtnDisplayed() {
        return isElementDisplayed(getRSEBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the RSE applications button is displayed
     *
     * @return true if the applications button is displayed, false otherwise
     */
    public boolean isRSEApplicationsBtnDisplayed() {
        return isElementDisplayed(getRSEBtn(ConfigureBtn.APPLICATIONS));
    }

    /**
     * Checks to see if the RSE create button is enabled
     *
     * @return true if the create button is enabled, false if it is disabled
     */
    public boolean isRSECreateBtnEnabled() {
        return isBtnEnabled(getRSEBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the RSE edit button is enabled
     *
     * @return true if the edit button is enabled, false if it is disabled
     */
    public boolean isRSEEditBtnEnabled() {
        return isBtnEnabled(getRSEBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks to see if the RSE delete button is enabled
     *
     * @return true if the delete button is enabled, false if it is disabled
     */
    public boolean isRSEDeleteBtnEnabled() {
        return isBtnEnabled(getRSEBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the RSE applications button is enabled
     *
     * @return true if the applications button is enabled, false if it is
     *         disabled
     */
    public boolean isRSEApplicationsBtnEnabled() {
        return isBtnEnabled(getRSEBtn(ConfigureBtn.APPLICATIONS));
    }

    /**
     * Checks to see if the given RSE attribute is displayed in the table
     *
     * @param rse -the RSE device expected
     * @return true if displayed, false otherwise
     */
    public boolean isRSEDisplayed(RSEDevice rse) {
        return isElementDisplayed(getRSERow(rse));
    }

    /**
     * Checks to see if the given RSE attribute is displayed in the table
     *
     * @param rse -the RSE device expected
     * @return true if displayed, false otherwise
     */
    public boolean isRSENameDisplayed(RSEDevice rse) {
        return isElementDisplayed(getRSENameCell(rse.getName()));
    }

    /**
     * Checks if the row for the given RSE is selected
     *
     * @param rse the RSE device to check
     * @return true if selected, false if de-selected
     */
    public boolean isRSERowSelected(RSEDevice rse) {
        String id = rse.getID();
        return isRowSelected(id);
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Create button
     *
     * @return the newly loaded Create RSE Device modal
     */
    public CreateRSEDeviceModal clickCreate() {
        getRSEBtn(ConfigureBtn.CREATE).click();
        return getPage(CreateRSEDeviceModal.class);
    }

    /**
     * Clicks the Delete button
     *
     * @return the Delete Warning modal
     */
    public ConfirmDeleteModal clickDeleteBtn() {
        getRSEBtn(ConfigureBtn.DELETE).click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the newly loaded Edit RSE Modal
     */
    public EditRSEModal clickEdit() {
        getRSEBtn(ConfigureBtn.EDIT).click();
        return getPage(EditRSEModal.class);
    }

    /**
     * Clicks the Applications button
     *
     * @return the newly loaded Applications Modal
     */
    public DeviceApplicationsModal clickApplications() {
        getRSEBtn(ConfigureBtn.APPLICATIONS).click();
        return getPage(DeviceApplicationsModal.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Verifies all expected buttons are displayed (Create, Edit, Delete, and
     * Applications)
     */
    public void checkBtns() {
        Assert.assertTrue("Create button not displayed as expected.", isRSECreateBtnDisplayed());
        Assert.assertTrue("Edit button not displayed as expected.", isRSEEditBtnDisplayed());
        Assert.assertTrue("Delete button not displayed as expected.", isRSEDeleteBtnDisplayed());
        Assert.assertTrue("Applications button not displayed as expected.", isRSEApplicationsBtnDisplayed());
    }

    /**
     * Verifies all column headers are displayed in cell towers table (ID,
     * Provider, X, Y, and Z)
     */
    public void checkRSEColumnHeaders() {
        Assert.assertTrue("The ID column header not displayed as expected in RSE table.", isRSEColumnHeaderDisplayed(RSEDeviceTableColumnHeader.ID));
        Assert.assertTrue("The Name column header not displayed as expected in RSE table.", isRSEColumnHeaderDisplayed(RSEDeviceTableColumnHeader.NAME));
        Assert.assertTrue("The X (cm) column header not displayed as expected in RSE table.", isRSEColumnHeaderDisplayed(RSEDeviceTableColumnHeader.X_COORDINATE));
        Assert.assertTrue("The Y (cm) column header not displayed as expected in RSE table.", isRSEColumnHeaderDisplayed(RSEDeviceTableColumnHeader.Y_COORDINATE));
        Assert.assertTrue("The Z (cm) column header not displayed as expected in RSE table.", isRSEColumnHeaderDisplayed(RSEDeviceTableColumnHeader.Z_COORDINATE));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(RSE_TABLE_COLUMN_XPATH_PREFIX + RSEDeviceTableColumnHeader.X_COORDINATE.label + "']"), 10);
    }

}
