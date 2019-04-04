package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal.ConfigureBtn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configuration of fixed cellular devices partial
 * page
 *
 * @author llaroussini
 */
public class ConfigureFixedCellularDevicePartialPage extends SimulationSettingsModal {

    /**
     * Default Constructor
     *
     * @param driver -the web driver
     */
    public ConfigureFixedCellularDevicePartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////
    // Enumerations
    ///////////////

    /**
     * Enumeration of columns in fixed cellular devices table
     *
     * @author llaroussini
     * @author rsmith
     */
    public enum FixedCellularDeviceTableColumnHeader {
        /**
         * ID column header
         */
        ID("ID", "fixedcellular-List-id"),

        /**
         * Device Name column header
         */
        NAME("Name", "fixedcellular-List-name"),

        /**
         * X Coordinate column header
         */
        X_COORDINATE("X (cm)", "fixedcellular-List-x"),

        /**
         * Z Coordinate column header
         */
        Y_COORDINATE("Y (cm)", "fixedcellular-List-y"),

        /**
         * Z Coordinate column header
         */
        Z_COORDINATE("Z (cm)", "fixedcellular-List-z"),

        /**
         * MAC Address column header
         */
        MAC_ADDRESS("MAC Address", "fixedcellular-List-mac-address");

        /**
         * The unique class name associated with the cell
         */
        private String uniqueClass;

        /**
         * The label of the column header as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label, sets the uniqueClass
         *
         * @param label The string to set as the label
         */
        FixedCellularDeviceTableColumnHeader(String label, String uniqueClass) {
            this.label = label;
            this.uniqueClass = uniqueClass;
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
     * Name of the Add Fixed Cellular Device button
     */
    private static final String CREATE_FIXED_CELLULAR_DEVICE_BTN_NAME = "Create Fixed Cellular Device";

    /**
     * Xpath prefix to buttons in Fixed Cellular Device tab
     */
    private static final String BTN_XPATH_PREFIX = "//a[contains(@id, 'fixed')][contains(@id, 'List')]//span[contains(@id, 'button')][text()='";

    /**
     * Xpath suffix to configuration button
     */
    private static final String CONFIGURE_DEVICE_BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Text displayed in class of element when disabled
     */
    private static final String DISABLED_CLASS_TEXT = "x-item-disabled";

    /**
     * Xpath of the Fixed Cellular Device table
     */
    private static final String FIXED_CELLULAR_TABLE_XPATH = "//div[contains(@id, 'fixed')]//table";

    /**
     * Xpath to the ID cell contained in a row
     */
    private static final String ID_CELL_XPATH = "./td[contains(@class, 'x-grid-cell-first')]//div";

    /**
     * Xpath prefix to Fixed Cellular table column
     */
    private static final String FIXED_CELLULAR_TABLE_COLUMN_XPATH_PREFIX = "//div[contains(@id, 'fixed')][contains(@id, 'Settings')]//span[@class='x-column-header-text-inner'][text()='";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the given column header element
     *
     * @param column -the column to get
     * @return the column header element
     */
    private El getFixedCellularDeviceColumnHeader(FixedCellularDeviceTableColumnHeader column) {
        return el(By.xpath(FIXED_CELLULAR_TABLE_COLUMN_XPATH_PREFIX + column.label + "']"));
    }

    /**
     * Gets the given fixed cellular button
     *
     * @param btn -the button to get
     * @return the cellular button
     */
    private El getFixedCellularBtn(ConfigureBtn btn) {
        return el(By.xpath(BTN_XPATH_PREFIX + btn.getLabel() + CONFIGURE_DEVICE_BTN_XPATH_SUFFIX));
    }

    /**
     * Gets the row associated with the given Fixed Cellular device
     *
     * @param fixed cell - the fixed cell device
     * @return the row element
     */
    private El getFixedCellularDeviceRow(FixedCellularDevice fixedcell) {
        String name = fixedcell.getName();
        return getRow(name);
    }

    /**
     * Gets the ID associated with the given Fixed Cellular device
     *
     * @param fixedcell -the fixed cell device
     * @return the ID as string
     */
    public String getFixedCellularID(FixedCellularDevice fixedcell) {
        El row = getFixedCellularDeviceRow(fixedcell);
        El idCell = row.el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
    }

    /**
     * Gets the value in the given cell for the given fixed cellular
     *
     * @param fixedcell -the fixed cellular device
     * @param column -the column from which to get the value
     * @return the value displayed in the given column for the given Fixed
     *         Cellular
     */
    public String getFixedCellValue(FixedCellularDevice fixedcell, FixedCellularDeviceTableColumnHeader column) {
        El row = getFixedCellularDeviceRow(fixedcell);
        El cell = row.el(By.xpath(CELL_IN_DEVICE_ROW_XPATH_PREFIX + column.uniqueClass + "')]/div"));
        return cell.getText();
    }

    /**
     * Gets the Fixed Cellular Devices table
     *
     * @return the table element
     */
    private El getFixedCellularTable() {
        return el(By.xpath(FIXED_CELLULAR_TABLE_XPATH));

    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the given fixed cellular device attribute is displayed
     * in the table
     *
     * @param attribute -the attribute associated with the fixed cellular device
     * @return true if displayed, false otherwise
     */
    public boolean isFixedCellularDeviceDisplayed(String attibute) {
        return isElementDisplayed(el(getCell(attibute)));
    }

    /**
     * Checks to see if the given fixed cellular device int attribute is
     * displayed in the table
     *
     * @param attirbute -the attribute associated with the fixed cellular device
     * @return true if displayed, false otherwise
     */
    public boolean isFixedCellularDeviceDisplayed(int attibute) {
        return isElementDisplayed(el(getCell(String.valueOf(attibute))));
    }

    /**
     * Checks to see if the column header expected is displayed
     *
     * @param column -the column expected
     * @return true if displayed, false otherwise
     */
    public boolean isFixedCellularDeviceColumnHeaderDisplayed(FixedCellularDeviceTableColumnHeader column) {
        return isElementDisplayed(getFixedCellularDeviceColumnHeader(column));
    }

    /**
     * Checks if Fixed Cellular table is displayed (only displays if devices are
     * present)
     *
     * @return true if displayed, false otherwise
     */
    public boolean areFixedCellularDevicesDisplayed() {
        return isElementDisplayed(getFixedCellularTable());
    }

    /**
     * Checks to see if the fixed cellular Applications button is displayed
     *
     * @return true if the fixed cellular configure button is displayed, false
     *         if it is not or cannot be found
     */
    public boolean isFixedCellularApplicationsBtnDisplayed() {
        return isElementDisplayed(getFixedCellularBtn(ConfigureBtn.APPLICATIONS));
    }

    /**
     * Checks to see if the fixed cellular edit button is displayed
     *
     * @return true if the fixed cellular edit button is displayed, false if it
     *         is not or cannot be found
     */
    public boolean isFixedCellularEditBtnDisplayed() {
        return isElementDisplayed(getFixedCellularBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks to see if the fixed cellular edit button is enabled
     *
     * @return true if the fixed cellular edit button is enabled, false if it is
     *         disabled
     */
    public boolean isFixedCellularEditBtnEnabled() {
        return isBtnEnabled(getFixedCellularBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks if the row for the given fixed cellular device is selected
     *
     * @param fixed cellular device to check
     * @return true if selected, false if de-selected
     */
    public boolean isFixedCellularDeviceRowSelected(FixedCellularDevice fixedcell) {
        String id = fixedcell.getID();
        return isRowSelected(id);
    }

    /**
     * Checks to see if the fixed cellular delete button is displayed
     *
     * @return true if the fixed cellular delete button is displayed, false if
     *         it is not or cannot be found
     */
    public boolean isFixedCellularDeleteBtnDisplayed() {
        return isElementDisplayed(getFixedCellularBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the fixed cellular delete button is enabled
     *
     * @return true if the fixed cellular delete button is enabled, false if it
     *         is disabled
     */
    public boolean isFixedCellularDeleteBtnEnabled() {
        return isBtnEnabled(getFixedCellularBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the fixed cellular Applications button is enabled
     *
     * @return true if the fixed cellular delete button is enabled, false if it
     *         is disabled
     */
    public boolean isFixedCellularApplicationsBtnEnabled() {
        return isBtnEnabled(getFixedCellularBtn(ConfigureBtn.APPLICATIONS));
    }

    /**
     * Checks to see if the Fixed Cellular Device create button is enabled
     *
     * @return true if the create button is enabled, false if it is disabled
     */
    public boolean isFixedCellularCreateBtnEnabled() {
        return isBtnEnabled(getFixedCellularBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the fixed cellular device create button is displayed
     *
     * @return true if create button is displayed, false otherwise
     */
    public boolean isFixedCellularDeviceCreateBtnDisplayed() {
        return isElementDisplayed(getFixedCellularBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the fixed cellular button is enabled
     *
     * @param btn -configure button to check
     * @return true if the fixed cellular button is enabled, false if it is
     *         disabled
     */
    public boolean isFixedCellularBtnEnabled(ConfigureBtn btn) {
        String displayedClass = getFixedCellularBtn(btn).getAttribute("class");
        return !displayedClass.contains(DISABLED_CLASS_TEXT);
    }

    /**
     * Verifies all expected buttons are displayed (Create, Edit, Delete, and
     * Applications)
     */
    public void checkBtns() {
        Assert.assertTrue("Create button not displayed as expected.", isFixedCellularDeviceCreateBtnDisplayed());
        Assert.assertTrue("Edit button not displayed as expected.", isFixedCellularEditBtnDisplayed());
        Assert.assertTrue("Delete button not displayed as expected.", isFixedCellularDeleteBtnDisplayed());
        Assert.assertTrue("Applications button not displayed as expected.", isFixedCellularApplicationsBtnDisplayed());
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Create Fixed Cellular Device button
     *
     * @return the newly loaded Add Fixed Cellular Device Form
     */
    public CreateFixedCellularDeviceModal clickCreateFixedCellularDeviceBtn() {
        getFixedCellularBtn(ConfigureBtn.CREATE).click();
        return getPage(CreateFixedCellularDeviceModal.class);
    }

    /**
     * Clicks the Applications button
     *
     * @return the newly loaded Applications Modal
     */
    public DeviceApplicationsModal clickApplications() {
        getFixedCellularBtn(ConfigureBtn.APPLICATIONS).click();
        return getPage(DeviceApplicationsModal.class);
    }

    /**
     * Clicks the Delete button
     *
     * @return the Delete Warning Form
     */
    public ConfirmDeleteModal clickDelete() {
        getFixedCellularBtn(ConfigureBtn.DELETE).click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the newly loaded Edit Fixed Cellular Device form
     */
    public EditFixedCellularDeviceModal clickEdit() {
        getFixedCellularBtn(ConfigureBtn.EDIT).click();
        return getPage(EditFixedCellularDeviceModal.class);
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Verifies all column headers are displayed in overview devices table (ID,
     * Name, MAC address, X Coordinate, Y Coordinate, and Z Coordinate)
     */
    public void checkFixedCellularDeviceColumnHeaders() {
        Assert.assertTrue("The ID column header not displayed as expected in Fixed Cellular table.", isFixedCellularDeviceColumnHeaderDisplayed(FixedCellularDeviceTableColumnHeader.ID));
        Assert.assertTrue("The Name column header not displayed as expected in Fixed Cellular table.", isFixedCellularDeviceColumnHeaderDisplayed(FixedCellularDeviceTableColumnHeader.NAME));
        Assert.assertTrue("The MAC Address column header not displayed as expected in Fixed Cellular table.",
                isFixedCellularDeviceColumnHeaderDisplayed(FixedCellularDeviceTableColumnHeader.MAC_ADDRESS));
        Assert.assertTrue("The X (cm) column header not displayed as expected in Fixed Cellular table.", isFixedCellularDeviceColumnHeaderDisplayed(FixedCellularDeviceTableColumnHeader.X_COORDINATE));
        Assert.assertTrue("The Y (cm) column header not displayed as expected in Fixed Cellular table.", isFixedCellularDeviceColumnHeaderDisplayed(FixedCellularDeviceTableColumnHeader.Y_COORDINATE));
        Assert.assertTrue("The Z (cm) column header not displayed as expected in Fixed Cellular table.", isFixedCellularDeviceColumnHeaderDisplayed(FixedCellularDeviceTableColumnHeader.Z_COORDINATE));
    }

    /**
     * Verifies the following buttons are disabled: Edit, Configure, and Delete
     */
    public void checkDisabledBtns() {
        Assert.assertFalse("Edit button is not disabled as expected.", isFixedCellularBtnEnabled(ConfigureBtn.EDIT));
        Assert.assertFalse("Configure button is not disabled as expected.", isFixedCellularBtnEnabled(ConfigureBtn.APPLICATIONS));
        Assert.assertFalse("Delete button is not disabled as expected.", isFixedCellularBtnEnabled(ConfigureBtn.DELETE));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(FIXED_CELLULAR_TABLE_COLUMN_XPATH_PREFIX + FixedCellularDeviceTableColumnHeader.MAC_ADDRESS.label + "']"), 20);
    }
}
