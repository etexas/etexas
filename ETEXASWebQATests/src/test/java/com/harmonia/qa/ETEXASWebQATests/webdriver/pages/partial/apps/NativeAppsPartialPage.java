package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial page class representing the User Native Apps page
 *
 * @author llaroussini
 */
public class NativeAppsPartialPage extends AppsPage {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public NativeAppsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Native App Columns
     *
     * @author llaroussini
     */
    public enum NativeAppColumn {
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
        DEVICE_TYPE("Device Type", "device-type-column"),
        /**
         * Command Line Column
         */
        CMD_LINE("Command Line", "command-line-column"),
        /**
         * Host Address Column
         */
        HOST_ADDR("Host Address", "host-address-column"),
        /**
         * Port Column
         */
        PORT("Port Number", "port-column");

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
        NativeAppColumn(String label, String uniqueClass) {
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
     * Xpath to Create button
     */
    private static final String CREATE_BTN_XPATH = "//div[contains(@id, 'native')]//span[text()='Create']/ancestor::a";

    /**
     * Xpath to Edit button
     */
    private static final String EDIT_BTN_XPATH = "//div[contains(@id, 'native')]//span[text()='Edit']/ancestor::a";

    /**
     * Xpath to Parameters button
     */
    private static final String PARAMETERS_BTN_XPATH = "//div[contains(@id, 'native')]//span[text()='Parameters']/ancestor::a";

    /**
     * Xpath to Delete button
     */
    private static final String DELETE_BTN_XPATH = "//div[contains(@id, 'native')]//span[text()='Delete']/ancestor::a";

    /**
     * Xpath prefix to column header cells
     */
    private static final String COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'native')]//span[text()='";

    /**
     * Xpath of the Native Apps table
     */
    private static final String NATIVE_APPS_TABLE_XPATH = "//div[contains(@id, 'native')]//table";

    /**
     * Xpath prefix of the Native Apps table row
     */
    private static final String NATIVE_APPS_ROW_XPATH_PREFIX = "//div[contains(@id, 'native')]//table//div[text()='";

    /**
     * Xpath to an ID of an application in the row
     */
    private static final String APP_ID_XPATH = "./td[contains(@class, 'x-grid-cell-first')]//div";

    /**
     * Partial xpath for accessing a div element from a td element containing
     * certain text
     */
    private static final String PARTIAL_TD_DIV = "./td/div[text()='";

    /**
     * Partial xpath for a table row ancestor
     */
    private static final String ANCESTOR_TR = "/ancestor::tr";

    /**
     * String constant for the 'class' attribute used throughout
     */
    private static final String ATTR_CLASS = "class";

    /**
     * Xpath prefix to cell in specific row of native app table
     */
    private static final String CELL_IN_NATIVE_APP_ROW_XPATH_PREFIX = ".//td[contains(@class, '";

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
     * Gets Parameters button
     *
     * @return the button element
     */
    private El getParametersBtn() {
        return el(By.xpath(PARAMETERS_BTN_XPATH));
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
     * Gets given app column header
     *
     * @param header -the native app column header to get
     * @return the column header cell element
     */
    private El getColumnHeader(NativeAppColumn header) {
        return el(By.xpath(COLUMN_HEADER_XPATH_PREFIX + header.getLabel() + "']"));
    }

    /**
     * Gets the Native Apps table
     *
     * @return the table element
     */
    private El getNativeAppsTable() {
        return el(By.xpath(NATIVE_APPS_TABLE_XPATH));
    }

    /**
     * Gets specific app row
     *
     * @param appName -the name of the app
     * @return the row element
     */
    private El getNativeAppRow(String appName) {
        return el(By.xpath(NATIVE_APPS_ROW_XPATH_PREFIX + appName + "']" + ANCESTOR_TR));
    }

    /**
     * Gets specific app row
     *
     * @param app -the app
     * @return the row element
     */
    private El getNativeAppRow(UserNativeApp app) {
        return getNativeAppRow(app.getName());
    }

    /**
     * Gets the ID of an app row
     *
     * @param appName - The String value of the application name
     * @return the ID of the application
     */
    private String getAppID(String appName) {
        El row = getNativeAppRow(appName);
        El ID = row.el(By.xpath(APP_ID_XPATH));
        return ID.getText();
    }

    /**
     * Gets app cell value from row
     *
     * @param appName - the String app name to retrieve a row from
     * @param value - the String value being searched for in the app row
     * @return the app name cell element
     */
    private El getNativeAppCell(String appName, String value) {
        El appRow = getNativeAppRow(appName);
        El cell = appRow.el(By.xpath(PARTIAL_TD_DIV + value + "']"));
        return cell;
    }

    /**
     * Gets the value in the given cell for the given native app
     *
     * @param nativeApp -the native app
     * @param column -the column from which to get the value
     * @return the value displayed in the given column for the given native app
     */
    public String getNativeAppCellValue(UserNativeApp nativeApp, NativeAppColumn column) {
        El row = getNativeAppRow(nativeApp);
        El cell = row.el(By.xpath(CELL_IN_NATIVE_APP_ROW_XPATH_PREFIX + column.uniqueClass + "')]/div"));
        return cell.getText();
    }

    /**
     * Gets the value in the given cell for the given native app
     *
     * @param nativeAppName -the name of the native app
     * @param column -the column from which to get the value
     * @return the value displayed in the given column for the given native app
     */
    public String getNativeAppCellValue(String nativeAppName, NativeAppColumn column) {
        El row = getNativeAppRow(nativeAppName);
        El cell = row.el(By.xpath(CELL_IN_NATIVE_APP_ROW_XPATH_PREFIX + column.uniqueClass + "')]/div"));
        return cell.getText();
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
        String btnClass = getCreateBtn().getAttribute(ATTR_CLASS);
        return btnClass.contains(DISABLED_BTN_CLASS);
    }

    /**
     * Checks to see if the Edit button is disabled
     *
     * @return True if the button is enabled, otherwise false
     */
    public boolean isEditBtnDisabled() {
        String btnClass = getEditBtn().getAttribute(ATTR_CLASS);
        return btnClass.contains(DISABLED_BTN_CLASS);
    }

    /**
     * Checks to see if the Parameters button is disabled
     *
     * @return True if the button is enabled, otherwise false
     */
    public boolean isParametersBtnDisabled() {
        String btnClass = getParametersBtn().getAttribute(ATTR_CLASS);
        return btnClass.contains(DISABLED_BTN_CLASS);
    }

    /**
     * Checks if the delete button is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isDeleteBtnDisabled() {
        String btnClass = getDeleteBtn().getAttribute(ATTR_CLASS);
        return btnClass.contains(DISABLED_BTN_CLASS);
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
     * Checks to see if the Parameters button is displayed
     *
     * @return true if button is displayed, false otherwise
     */
    public boolean isParametersBtnDisplayed() {
        return isElementDisplayed(getParametersBtn());
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
     * Checks to see if the given app column header cell is displayed
     *
     * @param header -the remote app column header expected
     * @return true if header cell displayed, false otherwise
     */
    public boolean isColumnHeaderCellDisplayed(NativeAppColumn header) {
        return isElementDisplayed(getColumnHeader(header));
    }

    /**
     * Checks if Native Apps table is displayed (only displays if apps are
     * present)
     *
     * @return true if displayed, false otherwise
     */
    public boolean areNativeAppsDisplayed() {
        return isElementDisplayed(getNativeAppsTable());
    }

    /**
     * Checks if Native Apps table is displayed (only displays if apps are
     * present)
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNativeAppsTableDisplayed() {
        return isElementDisplayed(getNativeAppsTable());
    }

    /**
     * Checks if row for app with given name is displayed
     *
     * @param appName -the name of the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(String appName) {
        return isElementDisplayed(getNativeAppRow(appName));
    }

    /**
     * Checks if row for given app is displayed
     *
     * @param app -the Native User App expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(UserNativeApp app) {
        String appName = app.getName();
        return isAppDisplayed(appName);
    }

    /**
     * Checks if a cell value exists given a specified app
     *
     * @param appName - the String name of the app being searched
     * @param value - the String value being checked for in the app row
     * @return true if the element is visible, otherwise false
     */
    public boolean isAppCellDisplayed(String appName, String value) {
        return isElementDisplayed(getNativeAppCell(appName, value));
    }

    /**
     * Checks for an ID to be set for an application row
     *
     * @param appName - the String value of the application name
     * @return true if ID is found, false otherwise
     */
    public boolean isAppIDSet(String appName) {
        String ID = getAppID(appName);
        if (ID.equals("") || ID == null) {
            return false;
        }
        else {
            return true;
        }
    }

    /**
     * Checks if native app row is selected
     *
     * @param nativeApp -the native app to check
     * @return true if row selected, false otherwise
     */
    public boolean isNativeAppRowSelected(UserNativeApp nativeApp) {
        El row = getNativeAppRow(nativeApp.getName()).el(By.xpath("./ancestor::table"));
        String selected = row.getAttribute("class");
        return selected.contains(SELECTED_ROW_CLASS);
    }

    ////////////
    // Utilities
    ///////////

    /**
     * Checks for display of create, delete, edit, and parameters buttons
     */
    public void checkBtnsDisplayed() {
        Assert.assertTrue("Create button could not be found.", isCreateBtnDisplayed());
        Assert.assertTrue("Edit button could not be foud.", isEditBtnDisplayed());
        Assert.assertTrue("Parameters button could not be found.", isParametersBtnDisplayed());
        Assert.assertTrue("Delete button could not be found.", isDeleteBtnDisplayed());
    }

    /**
     * Checks for display of the column header cells (App Name and Target
     * Device)
     */
    public void checkColumnHeaderCells() {
        Assert.assertTrue("ID column header cell not displayed as expected.", isColumnHeaderCellDisplayed(NativeAppColumn.ID));
        Assert.assertTrue("App Name column header cell not displayed as expected.", isColumnHeaderCellDisplayed(NativeAppColumn.NAME));
        Assert.assertTrue("Device Type column header cell not displayed as expected.", isColumnHeaderCellDisplayed(NativeAppColumn.DEVICE_TYPE));
        Assert.assertTrue("Command Line column header cell not displayed as expected.", isColumnHeaderCellDisplayed(NativeAppColumn.CMD_LINE));
        Assert.assertTrue("Host Address column header cell not displayed as expected.", isColumnHeaderCellDisplayed(NativeAppColumn.HOST_ADDR));
        Assert.assertTrue("Port column header cell not displayed as expected.", isColumnHeaderCellDisplayed(NativeAppColumn.PORT));
    }

    /**
     * Clicks the Create button
     *
     * @return the Create Native Application Modal
     */
    public CreateNativeApplicationModal clickCreate() {
        getCreateBtn().click();
        return getPage(CreateNativeApplicationModal.class);
    }

    /**
     * Clicks the Parameters button
     *
     * @return the Viewing App Parameters Modal
     */
    public ApplicationParametersModal clickParameters() {
        getParametersBtn().click();
        return getPage(ApplicationParametersModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the Edit Native Application modal
     */
    public EditNativeApplicationModal clickEdit() {
        getEditBtn().click();
        return getPage(EditNativeApplicationModal.class);
    }

    /**
     * Clicks the delete native app button
     *
     * @return the newly loaded Delete Warning Form
     */
    public ConfirmDeleteModal clickDelete() {
        getDeleteBtn().click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Selects given app name based on given boolean value
     *
     * @param appName -the name of the app to select
     * @param selected -true if app should be selected, false if not
     */
    public void selectNativeApp(String appName, boolean selected) {
        El appRow = getNativeAppRow(appName);
        boolean isSelected = appRow.isSelected();
        if (selected && !isSelected) {
            appRow.click();
        }
    }

    /**
     * Selects given app based on given boolean value
     *
     * @param app -the user native app to select
     * @param checked -true if app should be selected, false if not
     */
    public void selectNativeApp(UserNativeApp app, boolean checked) {
        String appName = app.getName();
        selectNativeApp(appName, checked);
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
        waitForElementToBeVisible(By.xpath(CREATE_BTN_XPATH), 20);
    }
}
