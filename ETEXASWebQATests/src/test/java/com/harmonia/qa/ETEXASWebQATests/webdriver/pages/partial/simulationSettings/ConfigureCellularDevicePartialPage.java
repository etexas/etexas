package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configuration of cellular devices partial page
 *
 * @author llaroussini
 */
public class ConfigureCellularDevicePartialPage extends CompositeSettingsModal {

    /**
     * Default Constructor
     *
     * @param driver -the web driver
     */
    public ConfigureCellularDevicePartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////
    // Enumerations
    ///////////////

    /**
     * Enumeration of columns in Cellular devices table
     *
     * @author llaroussini
     */
    public enum CellularDeviceTableColumnHeader {
        /**
         * ID column header
         */
        ID("ID"),
        /**
         * Device Name column header
         */
        DEVICE_NAME("Name"),
        /**
         * Min Quanity column header
         */
        MIN_QUANTITY("Min Quantity"),
        /**
         * Max Quantity column header
         */
        MAX_QUANTITY("Max Quantity"),
        /**
         * Cellular Percentage column header
         */
        PERCENTAGE("Percentage");

        /**
         * The label of the column header as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        CellularDeviceTableColumnHeader(String label) {
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
     * Xpath header for Cellular Device Profiles
     */
    private static final String CELLULAR_DEVICE_HEADER = "Cellular Device Profiles";

    /**
     * Xpath prefix to buttons in Cellular Device tab
     */
    private static final String BTN_XPATH_PREFIX = "//a[contains(@id, 'ETexas-device-view-cellular-List')]//span[contains(@id, 'ETexas-device-view-cellular-List')][text()='";

    /**
     * Xpath prefix to the Cellular Device Profiles column headers (ID, Name,
     * Min Quantity, Max Quantity, Percentage)
     */
    private static final String COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'ETexas-device-view-cellular-List')]/span[text()='";

    /**
     * Xpath to table containing Cellular Devices
     */
    private static final String DEVICE_TABLE_XPATH_PREFIX = "//div[contains(@id, 'ETexas-composite-view-Settings-cellular-grid-tab')]";

    /**
     * XPath to generic Cellular Devices table
     */
    private static final String DEVICE_TABLE_XPATH = DEVICE_TABLE_XPATH_PREFIX + "//table";

    /**
     * Xpath to a specific cell
     */
    private static final String CELL_XPATH = "//td[contains(@class, 'x-grid-cell-ETexas-device-view-cellular-List')]/div[text()='";

    /**
     * Xpath to a specific Cellular Device column
     */
    private static final String CELLULAR_DEVICE_COLUMN = "//tr[contains(@class, 'x-grid-row')]/td[contains(@class, 'x-grid-cell-ETexas-device-view-cellular-List')]/div[contains(text(), '";

    /**
     * Xpath for an ancestor row
     */
    private static final String ANCESTOR_ROW = "/ancestor::tr";

    /**
     * Xpath for an ancestor a
     */
    private static final String ANCESTOR_BTN = "/ancestor::a";

    /**
     * Xpath for Cellular Devices span text
     */
    private static final String CELLULAR_DEVICS_ACTIVE_TAB_SPAN = "[text()='";

    /**
     * Xpath for the ID of a cellular device row
     */
    private static final String DEVICE_ID_XPATH = "./td[contains(@class, 'x-grid-cell-ETexas-device-view-cellular-List-id-column')]/div";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the given column header element
     *
     * @param column -the column to get
     * @return the column header element
     */
    private El getCellularDeviceColumnHeader(CellularDeviceTableColumnHeader column) {
        return el(By.xpath(COLUMN_HEADER_XPATH_PREFIX + column.label + "']"));
    }

    /**
     * Gets the given cellular button
     *
     * @param btn -the button to get
     * @return the cellular button
     */
    private El getCellularBtn(ConfigureBtn btn) {
        return el(By.xpath(BTN_XPATH_PREFIX + btn.getLabel() + "']" + ANCESTOR_BTN));
    }

    /**
     * Gets the specified Cellular Device table row
     *
     * @param deviceName - the String name of the device row to return
     * @return the row element for a specific cellular device
     */
    private El getCellularDeviceRow(String deviceName) {
        return el(By.xpath(CELLULAR_DEVICE_COLUMN + deviceName + "')]" + ANCESTOR_ROW));
    }

    /**
     * Gets the element specified by the attribute value
     *
     * @param attribute - the String value being returned
     * @return the Element containing the attribute value
     */
    private El getCellularDeviceCell(String attribute) {
        return el(By.xpath(CELL_XPATH + attribute + "']"));
    }

    /**
     * Gets the ID element of a device with the given name
     *
     * @param deviceName - the String name of the device to retrive the ID for
     * @return - the Element containing the ID value
     */
    public El getCellularDeviceRowID(String deviceName) {
        El id = getCellularDeviceRow(deviceName).el((By.xpath(DEVICE_ID_XPATH)));
        return id;
    }

    /**
     * Gets the ID element of a device with the given name
     *
     * @param deviceName - the String name of the device to retrive the ID for
     * @return - the Element containing the ID value
     */
    public El getCellularDeviceRowID(CellularDevice device) {
        El row = getCellularDeviceRow(device.getName());
        El id = row.el((By.xpath(DEVICE_ID_XPATH)));
        return id;
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
    public boolean isCellularDeviceColumnHeaderDisplayed(CellularDeviceTableColumnHeader column) {
        return isElementDisplayed(getCellularDeviceColumnHeader(column));
    }

    /**
     * Checks to see if the cellular create button is displayed
     *
     * @return true if the cellular create button is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isCellularCreateBtnDisplayed() {
        return isElementDisplayed(getCellularBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the cellular Applications button is displayed
     *
     * @return true if the cellular Applications button is displayed, false if
     *         it is not or cannot be found
     */
    public boolean isCellularApplicationsBtnDisplayed() {
        return isElementDisplayed(getCellularBtn(ConfigureBtn.APPLICATIONS));
    }

    /**
     * Checks to see if the cellular edit button is displayed
     *
     * @return true if the cellular edit button is displayed, false if it is not
     *         or cannot be found
     */
    public boolean isCellularEditBtnDisplayed() {
        return isElementDisplayed(getCellularBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks to see if the cellular delete button is displayed
     *
     * @return true if the cellular delete button is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isCellularDeleteBtnDisplayed() {
        return isElementDisplayed(getCellularBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the cellular create button is enabled
     *
     * @return true if the cellular create button is enabled, false if it is
     *         disabled
     */
    public boolean isCellularCreateBtnEnabled() {
        El applicationsBtn = getCellularBtn(ConfigureBtn.CREATE);
        return isBtnEnabled(applicationsBtn);
    }

    /**
     * Checks to see if the cellular applications button is enabled
     *
     * @return true if the cellular applications button is enabled, false if it
     *         is disabled
     */
    public boolean isCellularApplicationsBtnEnabled() {
        El applicationsBtn = getCellularBtn(ConfigureBtn.APPLICATIONS);
        return isBtnEnabled(applicationsBtn);
    }

    /**
     * Checks to see if the cellular edit button is enabled
     *
     * @return true if the cellular edit button is enabled, false if it is
     *         disabled
     */
    public boolean isCellularEditBtnEnabled() {
        El editBtn = getCellularBtn(ConfigureBtn.EDIT);
        return isBtnEnabled(editBtn);
    }

    /**
     * Checks to see if the cellular delete button is enabled
     *
     * @return true if the cellular delete button is enabled, false if it is
     *         disabled
     */
    public boolean isCellularDeleteBtnEnabled() {
        El deleteBtn = getCellularBtn(ConfigureBtn.DELETE);
        return isBtnEnabled(deleteBtn);
    }

    /**
     * Checks for the presence of the Configure device and Delete buttons
     */
    public void checkCellularBtns() {
        Assert.assertTrue("The Create button was not found.", isCellularCreateBtnDisplayed());
        Assert.assertTrue("The Edit button was not found.", isCellularEditBtnDisplayed());
        Assert.assertTrue("The Delete button was not found.", isCellularDeleteBtnDisplayed());
        Assert.assertTrue("The Applications button was not found.", isCellularApplicationsBtnDisplayed());
    }

    /**
     * Checks for the presence of a table containing Cellular Devices returns
     * true if the table is displayed, otherwise false
     */
    public boolean isDeviceTableDisplayed() {
        return isElementDisplayed(By.xpath(DEVICE_TABLE_XPATH));
    }

    /**
     * Checks for the presence of the cellular device specific by the device
     * name
     *
     * @param deviceName - the String name of the device being checked
     * @return true if the element is displayed, otherwise false
     */
    public boolean isCellularDeviceDisplayed(String deviceName) {
        return isElementDisplayed(getCellularDeviceRow(deviceName));
    }

    /**
     * Checks for the presence of the specified attribute in a table cell
     *
     * @param attribute - the String attribute being checked
     * @return true if the element is displayed, otherwise false
     */
    public boolean isCellularDeviceAttributeDisplayed(String attribute) {
        return isElementDisplayed(getCellularDeviceCell(attribute));
    }

    /**
     * Checks if the ID of a device is set and displayed
     *
     * @param deviceName - the String name of the device row being checked
     * @return true if the element is displayed, otherwise false
     */
    public boolean isCellularDeviceIdDisplayed(String deviceName) {
        El rowID = getCellularDeviceRowID(deviceName);
        if (null == rowID || "".equals(rowID.getText())) {
            return false;
        }
        else {
            return isElementDisplayed(rowID);
        }
    }

    /**
     * Checks if the desired row is currently selected
     *
     * @param deviceName - the String name of the device being checked
     * @return true if the element is selected, otherwise false
     */
    public boolean isCellularRowSelected(String deviceName) {
        Assert.assertNotNull("The selection status of the row could not be determined because the row can't be located.", getCellularDeviceRow(deviceName));
        El row = getCellularDeviceRow(deviceName);
        boolean selected = false;
        if (!"true".equals(row.getAttribute("aria-selected"))) {
            selected = false;
        }
        else {
            selected = true;
        }
        return selected;
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Create Cellular Device button
     *
     * @return the newly loaded Create Cellular Device Form
     */
    public CreateCellularDeviceModal clickCreateCellularDeviceBtn() {
        getCellularBtn(ConfigureBtn.CREATE).click();
        return getPage(CreateCellularDeviceModal.class);
    }

    /**
     * Clicks the Delete button
     *
     * @return the Delete Warning Form
     */
    public ConfirmDeleteModal clickDeleteBtn() {
        getCellularBtn(ConfigureBtn.DELETE).click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the newly loaded Edit Cellular Device form
     */
    public EditCellularDeviceModal clickEdit() {
        getCellularBtn(ConfigureBtn.EDIT).click();
        return getPage(EditCellularDeviceModal.class);
    }

    /**
     * Clicks the Application button
     *
     * @return the newly loaded Device Applications form
     */
    public DeviceApplicationsModal clickApplications() {
        getCellularBtn(ConfigureBtn.APPLICATIONS).click();
        return getPage(DeviceApplicationsModal.class);
    }

    /**
     * Selects the cellular device if it has not already been selected
     *
     * @param device - the device being selected
     */
    public void selectCellularDevice(CellularDevice device) {
        String deviceName = device.getName();
        selectRow(deviceName, true);
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Verifies all column headers are displayed in overview devices table (ID,
     * Device Name, Device Type)
     */
    public void checkCellularDeviceColumnHeaders() {
        Assert.assertTrue("The ID column header not displayed as expected in Cellular Devices table.", isCellularDeviceColumnHeaderDisplayed(CellularDeviceTableColumnHeader.ID));
        Assert.assertTrue("The Device Name column header not displayed as expected in Cellular Devices table.", isCellularDeviceColumnHeaderDisplayed(CellularDeviceTableColumnHeader.DEVICE_NAME));
        Assert.assertTrue("The Device Min Quantity column header not displayed as expected in the Cellular Devices table.",
                isCellularDeviceColumnHeaderDisplayed(CellularDeviceTableColumnHeader.MIN_QUANTITY));
        Assert.assertTrue("The Device Max Quantity column header not displayed as expected in the Cellular Devices table.",
                isCellularDeviceColumnHeaderDisplayed(CellularDeviceTableColumnHeader.MAX_QUANTITY));
        Assert.assertTrue("The Percentage column header not displayed as expected in Cellular Devices table.", isCellularDeviceColumnHeaderDisplayed(CellularDeviceTableColumnHeader.PERCENTAGE));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(DEVICE_TABLE_XPATH), 10);
    }

}
