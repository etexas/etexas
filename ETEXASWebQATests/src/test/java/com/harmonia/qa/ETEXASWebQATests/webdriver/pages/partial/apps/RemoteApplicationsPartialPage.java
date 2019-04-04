package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial page class representing the User Remote Apps page
 *
 * @author llaroussini
 */
public class RemoteApplicationsPartialPage extends AppsPage {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public RemoteApplicationsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Remote App Columns
     *
     * @author rsmith
     */
    public enum RemoteAppColumn {
        /**
         * Name Column
         */
        NAME("Name", "name-column"),
        /**
         * ID Column
         */
        ID("ID", "id-column"),
        /**
         * Device Type Column
         */
        DEVICE_TYPE("Device Type", "device-type-column");

        /**
         * The label of the column headers as displayed in the UI
         */
        private String label;

        /**
         * The unique class associated with the columns as displayed in the UI
         */
        private String uniqueClass;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         * @param uniqueClass The unique class associated with the column
         */
        RemoteAppColumn(String label, String uniqueClass) {
            this.label = label;
            this.uniqueClass = uniqueClass;
        }

        /**
         * Gets the label of the column header as displayed in the UI
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
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath prefix for all buttons in Remote Applications Tab
     */
    private static final String BTN_XPATH_PREFIX = "//div[contains(@id, 'remote-application-profile-toolbar')]//span[text()='";

    /**
     * Xpath suffix for all buttons in Remote Applications Tab
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to Create button
     */
    private static final String CREATE_BTN_XPATH = BTN_XPATH_PREFIX + "Create" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to Edit button
     */
    private static final String EDIT_BTN_XPATH = BTN_XPATH_PREFIX + "Edit" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to Delete button
     */
    private static final String DELETE_BTN_XPATH = BTN_XPATH_PREFIX + "Delete" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to Parameters button
     */
    private static final String PARAMETERS_BTN_XPATH = BTN_XPATH_PREFIX + "Parameters" + BTN_XPATH_SUFFIX;

    /**
     * Xpath prefix to column header cells
     */
    private static final String COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'remote')]//span[text()='";

    /**
     * Xpath of the Remote Apps table
     */
    private static final String REMOTE_APPS_TABLE_XPATH = "//div[contains(@id, 'remote')]//table";

    /**
     * Xpath prefix of the Remote Apps table row
     */
    private static final String REMOTE_APPS_ROW_XPATH_PREFIX = "//div[contains(@id, 'remote')]//table//div[text()='";

    /**
     * Xpath to ID cell, used in conjunction with row element (finds ID cell in
     * a given row when used together)
     */
    private static final String ID_CELL_XPATH = "./ancestor::tbody//td[contains(@class, 'x-grid-cell-first')]";

    /**
     * Xpath prefix assigned to name cell
     */
    private static final String NAME_CELL_XPATH_PREFIX = "//div[contains(@class, 'x-grid-cell-inner ')][text()='";

    /**
     * Xpath to Type cell, used in conjunction with row element (finds ID cell
     * in a given row when used together)
     */
    private static final String TYPE_CELL_XPATH = "./ancestor::tbody//td[contains(@class, 'type')]//div";

    /**
     * Xpath prefix to cell in specific row of remote app table
     */
    private static final String CELL_IN_REMOTE_APP_ROW_XPATH_PREFIX = "//div[contains(@id, 'remote-application-profile-grid')]//td[contains(@class, '";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets Create button
     *
     * @return the button element
     */
    private El getCreateBtn() {
        return el(By.xpath(CREATE_BTN_XPATH));
    }

    /**
     * Gets Edit button
     *
     * @return the button element
     */
    private El getEditBtn() {
        return el(By.xpath(EDIT_BTN_XPATH));
    }

    /**
     * Gets Delete button
     *
     * @return the button element
     */
    private El getDeleteBtn() {
        return el(By.xpath(DELETE_BTN_XPATH));
    }

    /**
     * Gets Parameters button
     *
     * @return the button element
     */
    private El getParametersBtn() {
        return el(By.xpath(PARAMETERS_BTN_XPATH));
    }

    /**
     * Gets given app column header
     *
     * @param header -the remote app column header to get
     * @return the column header cell element
     */
    private El getColumnHeader(AppColumnHeader header) {
        return el(By.xpath(COLUMN_HEADER_XPATH_PREFIX + header.getLabel() + "']"));
    }

    /**
     * Gets the Remote Apps table
     *
     * @return the table element
     */
    private El getRemoteAppsTable() {
        return el(By.xpath(REMOTE_APPS_TABLE_XPATH));
    }

    /**
     * Gets the value in the given cell for the given remote app
     *
     * @param remoteAppName -the name of the remote app
     * @param deviceType -the column from which to get the value
     * @return the value displayed in the given column for the given remote app
     */
    public String getRemoteAppCellValue(UserRemoteApp remoteApp, RemoteAppColumn column) {
        String remoteAppName = remoteApp.getName();
        return getRemoteAppCellValue(remoteAppName, column);
    }

    /**
     * Gets the value in the given cell for the given remote app
     *
     * @param remoteAppName -the name of the remote app
     * @param deviceType -the column from which to get the value
     * @return the value displayed in the given column for the given remote app
     */
    public String getRemoteAppCellValue(String remoteAppName, RemoteAppColumn column) {
        El row = getRemoteAppRow(remoteAppName);
        El cell = row.el(By.xpath(CELL_IN_REMOTE_APP_ROW_XPATH_PREFIX + column.uniqueClass + "')]/div"));
        return cell.getText();
    }

    /**
     * Gets specific app row
     *
     * @return the row element
     */
    private El getRemoteAppRow(UserRemoteApp app) {
        String appName = app.getName();
        return getRemoteAppRow(appName);
    }

    /**
     * Gets specific app row
     *
     * @return the row element
     */
    private El getRemoteAppRow(String appName) {
        return el(By.xpath(REMOTE_APPS_ROW_XPATH_PREFIX + appName + "']/ancestor::tr"));
    }

    /**
     * Gets cell in table with given remote app name
     *
     * @param appName -the app name expected
     * @return the cell element
     */
    private El getRemoteAppNameCell(String appName) {
        return el(By.xpath(NAME_CELL_XPATH_PREFIX + appName + "']"));
    }

    /**
     * Gets displayed ID for given remote app
     *
     * @param remoteApp -the remote app
     * @return the ID as string
     */
    public String getRemoteAppID(UserRemoteApp remoteApp) {
        El idCell = getRemoteAppRow(remoteApp).el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
    }

    /**
     * Gets displayed Device Type for given remote app
     *
     * @param remoteApp -the remote app
     * @return the ID as string
     */
    public String getDeviceType(UserRemoteApp remoteApp) {
        El typeCell = getRemoteAppRow(remoteApp).el(By.xpath(TYPE_CELL_XPATH));
        return typeCell.getText();
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks if the create button is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isCreateBtnDisabled() {
        return isBtnDisabled(getCreateBtn());
    }

    /**
     * Checks if the edit button is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isEditBtnDisabled() {
        return isBtnDisabled(getEditBtn());
    }

    /**
     * Checks if the delete button is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isDeleteBtnDisabled() {
        return isBtnDisabled(getDeleteBtn());
    }

    /**
     * Checks if the parameters button is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isParametersBtnDisabled() {
        return isBtnDisabled(getParametersBtn());
    }

    /**
     * Checks to see if the Create button is displayed
     *
     * @return true if button is displayed, false otherwise
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getCreateBtn());
    }

    /**
     * Checks to see if the Edit button is displayed
     *
     * @return true if button is displayed, false otherwise
     */
    public boolean isEditBtnDisplayed() {
        return isElementDisplayed(getEditBtn());
    }

    /**
     * Checks to see if the Delete button is displayed
     *
     * @return true if button is displayed, false otherwise
     */
    public boolean isDeleteBtnDisplayed() {
        return isElementDisplayed(getDeleteBtn());
    }

    /**
     * Checks to see if the Parameters button is displayed
     *
     * @return true if button is displayed, false otherwise
     */
    public boolean isParametersBtnDisplayed() {
        return isElementDisplayed(getDeleteBtn());
    }

    /**
     * Checks to see if the given app column header cell is displayed
     *
     * @param header -the remote app column header expected
     * @return true if header cell displayed, false otherwise
     */
    public boolean isColumnHeaderCellDisplayed(AppColumnHeader header) {
        return isElementDisplayed(getColumnHeader(header));
    }

    /**
     * Checks if Remote Apps table is displayed (only displays if apps are
     * present)
     *
     * @return true if displayed, false otherwise
     */
    public boolean areRemoteAppsDisplayed() {
        return isElementDisplayed(getRemoteAppsTable());
    }

    /**
     * Checks if row for app with given name is displayed
     *
     * @param UserRemoteApp -the name of the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(UserRemoteApp app) {
        return isElementDisplayed(getRemoteAppRow(app));
    }

    /**
     * Checks if row for app with given name is displayed
     *
     * @param appName -the name of the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(String appName) {
        return isElementDisplayed(getRemoteAppRow(appName));
    }

    /**
     * Checks to see if the given app name is displayed in the table
     *
     * @param appName -name of the app expected
     * @return true if displayed, false if not
     */
    public boolean isAppNameDisplayed(String appName) {
        return isElementDisplayed(getRemoteAppNameCell(appName));
    }

    /**
     * Checks if remote app row is selected
     *
     * @param remoteApp -the remote app to check
     * @return true if row selected, false otherwise
     */
    public boolean isRemoteAppRowSelected(UserRemoteApp remoteApp) {
        El row = getRemoteAppRow(remoteApp).el(By.xpath("./ancestor::table"));
        String selected = row.getAttribute("class");
        return selected.contains(SELECTED_ROW_CLASS);
    }

    ////////////
    // Utilities
    ///////////

    /**
     * Checks for display of create, edit, delete, and parameters buttons
     */
    public void checkBtnsDisplayed() {
        Assert.assertTrue("Create button could not be found.", isCreateBtnDisplayed());
        Assert.assertTrue("Edit button could not be found.", isEditBtnDisplayed());
        Assert.assertTrue("Delete button could not be found.", isDeleteBtnDisplayed());
        Assert.assertTrue("Parameters button could not be found.", isParametersBtnDisplayed());
    }

    /**
     * Checks for display of the column header cells (ID, Name, and Device Type)
     */
    public void checkColumnHeaderCells() {
        Assert.assertTrue("ID column header cell not displayed as expected.", isColumnHeaderCellDisplayed(AppColumnHeader.APP_ID));
        Assert.assertTrue("Name column header cell not displayed as expected.", isColumnHeaderCellDisplayed(AppColumnHeader.APP_NAME));
        Assert.assertTrue("Device Type column header cell not displayed as expected.", isColumnHeaderCellDisplayed(AppColumnHeader.DEVICE_TYPE));
    }

    /**
     * Clicks the Create button
     *
     * @return the Create Remote Application Modal
     */
    public CreateRemoteApplicationModal clickCreate() {
        getCreateBtn().click();
        return getPage(CreateRemoteApplicationModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the Edit Remote Application modal
     */
    public EditRemoteApplicationModal clickEditBtn() {
        getEditBtn().click();
        return getPage(EditRemoteApplicationModal.class);
    }

    /**
     * Clicks the delete button
     *
     * @return the newly loaded Delete Warning Form
     */
    public ConfirmDeleteModal clickDelete() {
        getDeleteBtn().click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Clicks the Parameters button
     *
     * @return the newly loaded Viewing App Parameters Window
     */
    public ApplicationParametersModal clickParameters() {
        getParametersBtn().click();
        return getPage(ApplicationParametersModal.class);
    }

    /**
     * Selects given app based on given boolean value
     *
     * @param app -the user remote app to select
     * @param checked -true if app should be selected, false if not
     */
    public void selectRemoteApp(UserRemoteApp app, boolean selected) {
        String appName = app.getName();
        selectRemoteApp(appName, selected);
    }

    /**
     * Selects given app name based on given boolean value
     *
     * @param appName -the name of the app to select
     * @param selected -true if app should be selected, false if not
     */
    public void selectRemoteApp(String appName, boolean selected) {
        El appRow = getRemoteAppRow(appName);
        boolean isSelected = appRow.isSelected();
        if (selected && !isSelected) {
            appRow.click();
        }
        waitForElementToBeEnabled(By.xpath(DELETE_BTN_XPATH));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(CREATE_BTN_XPATH));
    }
}