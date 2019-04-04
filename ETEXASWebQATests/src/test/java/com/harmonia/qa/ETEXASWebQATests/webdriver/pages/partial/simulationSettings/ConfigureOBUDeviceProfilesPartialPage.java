package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditOBUDeviceProfileModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configuration of OBU devices partial page
 *
 * @author llaroussini
 */
public class ConfigureOBUDeviceProfilesPartialPage extends CompositeSettingsModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public ConfigureOBUDeviceProfilesPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////
    //Enumerations
    //////////////

    /**
     * Enumeration of cell columns in OBU devices table
     *
     * @author llaroussini
     */
    public enum OBUDeviceTableColumn {
        /**
         * ID column header
         */
        ID("ID", "device-view-List-id"),
        /**
         * Name column header
         */
        NAME("Name", "device-view-List-name"),
        /**
         * Percentage column header
         */
        PERCENTAGE("Percentage", "device-view-List-percentage");

        /**
         * The label of the column header as appears in the application
         */
        private String label;

        /**
         * The unique class name associated with the cell
         */
        private String uniqueClass;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         * @param uniqueClass The unique class associated with the column
         */
        OBUDeviceTableColumn(String label, String uniqueClass) {
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

        /**
         * Gets the unique class associated with the column
         *
         * @return The unique class associated with the column
         */
        public String getUniqueClass() {
            return this.uniqueClass;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Xpath prefix to OBU table column
     */
    private static final String OBU_TABLE_COLUMN_XPATH_PREFIX = "//div[contains(@id, 'obu')]//span[@class='x-column-header-text-inner'][text()='";

    /**
     * Xpath prefix to buttons in OBU Device tab
     */
    private static final String CONFIGURE_OBU_BTN_XPATH_PREFIX = "//a[contains(@id, 'obu')][contains(@id, 'List')]//span[contains(@id, 'button')][text()='";

    /**
     * Xpath prefix to a specific cell (assumes this will be used in conjunction
     * with a row)
     */
    private static final String CELL_XPATH_PREFIX = ".//td[contains(@class, '";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the given column header element
     *
     * @param column -the column to get
     * @return the column header element
     */
    private El getOBUColumnHeader(OBUDeviceTableColumn column) {
        return el(By.xpath(OBU_TABLE_COLUMN_XPATH_PREFIX + column.label + "']"));
    }

    /**
     * Gets the given button
     *
     * @param btn -the button to get
     * @return the button
     */
    private El getOBUBtn(ConfigureBtn btn) {
        return el(By.xpath(CONFIGURE_OBU_BTN_XPATH_PREFIX + btn.getLabel() + "']/ancestor::a"));
    }

    /**
     * Gets the cell associated with the given OBU attribute
     *
     * @param attribute -UNIQUE attribute associated with OBU
     * @return the cell element
     */
    private El getOBUCell(String attribute) {
        return el(getCell(attribute));
    }

    /**
     * Gets the row associated with the given OBU attribute
     *
     * @param attribute -UNIQUE attribute associated with OBU
     * @return the row element
     */
    private El getOBURow(String attribute) {
        El cell = getOBUCell(attribute);
        return cell.el(By.xpath("./ancestor::tr"));
    }

    /**
     * Gets the row associated with the given OBU (uses the name attribute of
     * the device to find the row)
     *
     * @param obu -the obu device
     * @return the row element
     */
    private El getOBURow(OBUDevice obu) {
        String name = obu.getName();
        return getOBURow(name);
    }

    /**
     * Gets the value in the given cell for the given OBU
     *
     * @param obu -the obu device
     * @param column -the column from which to get the value
     * @return the value displayed in the given column for the given OBU
     */
    public String getOBUCellValue(OBUDevice obu, OBUDeviceTableColumn column) {
        El row = getOBURow(obu);
        El cell = row.el(By.xpath(CELL_XPATH_PREFIX + column.uniqueClass + "')]/div"));
        return cell.getText();
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
    public boolean isOBUDeviceColumnHeaderDisplayed(OBUDeviceTableColumn column) {
        return isElementDisplayed(getOBUColumnHeader(column));
    }

    /**
     * Checks to see if the OBU create button is displayed
     *
     * @return true if OBU create button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isOBUCreateBtnDisplayed() {
        return isElementDisplayed(getOBUBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the OBU edit button is displayed
     *
     * @return true if the OBU edit button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isOBUEditBtnDisplayed() {
        return isElementDisplayed(getOBUBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks to see if the OBU delete button is displayed
     *
     * @return true if the OBU delete button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isOBUDeleteBtnDisplayed() {
        return isElementDisplayed(getOBUBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the OBU applications button is displayed
     *
     * @return true if the OBU applications button is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isOBUApplicationsBtnDisplayed() {
        return isElementDisplayed(getOBUBtn(ConfigureBtn.APPLICATIONS));
    }

    /**
     * Checks to see if the OBU create button is enabled
     *
     * @return true if the OBU create button is enabled, false if it is disabled
     */
    public boolean isOBUCreateBtnEnabled() {
        return isBtnEnabled(getOBUBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the OBU edit button is enabled
     *
     * @return true if the OBU edit button is enabled, false if it is disabled
     */
    public boolean isOBUEditBtnEnabled() {
        return isBtnEnabled(getOBUBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks if OBU delete button is enabled
     *
     * @return true if the OBU create button is enabled, false if it is disabled
     */
    public boolean isOBUDeleteBtnEnabled() {
        return isBtnEnabled(getOBUBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the OBU applications button is enabled
     *
     * @return true if the OBU applications button is enabled, false if it is
     *         disabled
     */
    public boolean isOBUApplicationsBtnEnabled() {
        return isBtnEnabled(getOBUBtn(ConfigureBtn.APPLICATIONS));
    }

    /**
     * Checks if OBU device is displayed
     *
     * @param obu -the OBU device expected
     * @return true if device row is found, false otherwise
     */
    public boolean isOBUDisplayed(OBUDevice obu) {
        return isElementDisplayed(getOBUCell(obu.getName()));
    }

    /**
     * Checks if OBU device with given unique attribute is displayed
     *
     * @param attribute - unique attribute of OBU expected
     * @return true if device row is found, false otherwise
     */
    public boolean isOBUDisplayed(String attribute) {
        return isElementDisplayed(getOBUCell(attribute));
    }

    /**
     * Checks if the row for the given OBU is selected
     *
     * @param obu the OBU to check
     * @return true if selected, false if de-selected
     */
    public boolean isOBURowSelected(OBUDevice obu) {
        String id = getOBUCellValue(obu, OBUDeviceTableColumn.ID);
        return isRowSelected(id);
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Create button
     *
     * @return the newly loaded Create OBU Device Profile Modal
     */
    public CreateOBUDeviceProfileModal clickCreate() {
        getOBUBtn(ConfigureBtn.CREATE).click();
        return getPage(CreateOBUDeviceProfileModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the newly loaded Edit OBU Device Profile Modal
     */
    public EditOBUDeviceProfileModal clickEdit() {
        getOBUBtn(ConfigureBtn.EDIT).click();
        return getPage(EditOBUDeviceProfileModal.class);
    }

    /**
     * * Clicks the Applications button
     *
     * @return the newly loaded Device Applications Modal
     */
    public DeviceApplicationsModal clickApplications() {
        getOBUBtn(ConfigureBtn.APPLICATIONS).click();
        return getPage(DeviceApplicationsModal.class);
    }

    /**
     * Clicks the Delete button
     *
     * @return the Delete Warning Form
     */
    public ConfirmDeleteModal clickDeleteBtn() {
        getOBUBtn(ConfigureBtn.DELETE).click();
        return getPage(ConfirmDeleteModal.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Create, Edit, Delete, and Applications
     * buttons
     */
    public void checkOBUBtns() {
        Assert.assertTrue("The Create button was not found in the OBU Device Profiles tab.", isOBUCreateBtnDisplayed());
        Assert.assertTrue("The Edit button was not found in the OBU Device Profiles tab.", isOBUEditBtnDisplayed());
        Assert.assertTrue("The Delete button was not found in the OBU Device Profiles tab.", isOBUDeleteBtnDisplayed());
        Assert.assertTrue("The Applications button was not found in the OBU Device Profiles tab.", isOBUApplicationsBtnDisplayed());
    }

    /**
     * Verifies all column headers are displayed in devices table (ID, Name,
     * Percentage)
     */
    public void checkOBUDeviceColumnHeaders() {
        Assert.assertTrue("The ID column header not displayed as expected in OBU Devices table.", isOBUDeviceColumnHeaderDisplayed(OBUDeviceTableColumn.ID));
        Assert.assertTrue("The Name column header not displayed as expected in OBU Devices table.", isOBUDeviceColumnHeaderDisplayed(OBUDeviceTableColumn.NAME));
        Assert.assertTrue("The Percentage column header not displayed as expected in OBU Devices table.", isOBUDeviceColumnHeaderDisplayed(OBUDeviceTableColumn.PERCENTAGE));
    }
}
