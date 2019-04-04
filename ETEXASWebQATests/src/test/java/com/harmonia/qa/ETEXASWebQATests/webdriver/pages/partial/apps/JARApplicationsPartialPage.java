package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial page class representing the JAR Applications page
 *
 * @author llaroussini
 */
public class JARApplicationsPartialPage extends AppsPage {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public JARApplicationsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath prefix for all buttons in JAR Applications Tab
     */
    private static final String BTN_XPATH_PREFIX = "//div[contains(@id, 'jar-application-profile-toolbar')]//span[text()='";

    /**
     * Xpath suffix for all buttons in JAR Applications Tab
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
    private static final String COLUMN_HEADER_XPATH_PREFIX = "//div[contains(@id, 'jar')]//span[text()='";

    /**
     * Class of disabled button
     */
    private static final String DISABLED_BTN_CLASS = "x-btn-disabled";

    /**
     * Xpath of the JAR Apps table
     */
    private static final String JAR_APPS_TABLE_XPATH = "//div[contains(@id, 'jargridpanel')]//table";

    /**
     * Xpath prefix to JAR row
     */
    private static final String JAR_ROW_XPATH_PREFIX = "//div[contains(@id, 'jar')]//table//div[text()='";

    /**
     * Xpath suffix to JAR row
     */
    private static final String JAR_ROW_XPATH_SUFFIX = "']/ancestor::tr";

    /**
     * Xpath prefix of the App in JAR table row
     */
    private static final String APP_IN_JAR_ROW_XPATH_PREFIX = "//div[contains(@id, 'jar-application-profile-grid')]//table//div[text()='";

    /**
     * Xpath to ID cell, used in conjunction with row element (finds ID cell in
     * a given row when used together)
     */
    private static final String ID_CELL_XPATH = "./ancestor::tbody//td[contains(@class, 'x-grid-cell-first')]";

    /**
     * Xpath prefix assigned to name cell
     */
    private static final String NAME_CELL_XPATH_PREFIX = "//div[@class='x-grid-group-title'][text()='";

    /**
     * Xpath to Type cell, used in conjunction with row element (finds ID cell
     * in a given row when used together)
     */
    private static final String TYPE_CELL_XPATH = "./ancestor::tbody//td[contains(@class, 'device-type-column')]//div";

    /**
     * Icon used to expand a JAR of Applications
     */
    private static final String JAR_EXPAND_ICON_XPATH = ".//div[contains(@data-qtip, 'Click')]";

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
     * @param header -the native app column header to get
     * @return the column header cell element
     */
    private El getColumnHeader(AppColumnHeader header) {
        return el(By.xpath(COLUMN_HEADER_XPATH_PREFIX + header.getLabel() + "']"));
    }

    /**
     * Gets the JAR Apps table
     *
     * @return the table element
     */
    private El getJARAppsTable() {
        return el(By.xpath(JAR_APPS_TABLE_XPATH));
    }

    /**
     * Gets the row of the given JAR
     *
     * @param jarName -the JAR name
     * @return the row element
     */
    private El getJARRow(String jarName) {
        return el(By.xpath(JAR_ROW_XPATH_PREFIX + jarName + JAR_ROW_XPATH_SUFFIX));
    }

    /**
     * Gets specific app row
     *
     * @param appName -the name of the app in the row
     * @return the row element
     */
    private El getAppInJARRow(String appName) {
        return el(By.xpath(APP_IN_JAR_ROW_XPATH_PREFIX + appName + "']/ancestor::tr"));
    }

    /**
     * Gets cell in table with given JAR app name
     *
     * @param appName -the app name expected
     * @return the cell element
     */
    private El getJARAppNameCell(String appName) {
        return el(By.xpath(NAME_CELL_XPATH_PREFIX + appName + "']"));
    }

    /**
     * Gets the expand icon associated with given JAR
     *
     * @param app -the JAR app expected to have expand icon
     * @return the expand icon element
     */
    private El getExpandCollapseIcon(UserJarApp app) {
        El row = getJARRow(app.getName());
        return row.el(By.xpath(JAR_EXPAND_ICON_XPATH));
    }

    /**
     * Gets displayed ID for given JAR app
     *
     * @param appName -the name of the app
     * @return the ID as string
     */
    public String getJARAppID(String appName) {
        El idCell = getAppInJARRow(appName).el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
    }

    /**
     * Gets displayed Device Type for given JAR app
     *
     * @param appName -name of app within the JAR
     * @return the device type as string
     */
    public String getDeviceType(String appName) {
        El typeCell = getAppInJARRow(appName).el(By.xpath(TYPE_CELL_XPATH));
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
     * Checks to see if expand icon is displayed with the given JAR app
     *
     * @param app -the app where expand icon is expected
     * @return true if icon is displayed, false otherwise
     */
    public boolean isExpandIconDisplayed(UserJarApp app) {
        El icon = getExpandCollapseIcon(app);
        String status = icon.getAttribute("data-qtip");
        return status.contains("expand");
    }

    /**
     * Checks to see if collapse icon is displayed with the given JAR app
     *
     * @param app -the app where expand icon is expected
     * @return true if icon is displayed, false otherwise
     */
    public boolean isCollapseIconDisplayed(UserJarApp app) {
        El icon = getExpandCollapseIcon(app);
        String status = icon.getAttribute("data-qtip");
        return status.contains("collapse");
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
     * @param header -the JAR app column header expected
     * @return true if header cell displayed, false otherwise
     */
    public boolean isColumnHeaderCellDisplayed(AppColumnHeader header) {
        return isElementDisplayed(getColumnHeader(header));
    }

    /**
     * Checks if JAR Apps table is displayed (only displays if apps are present)
     *
     * @return true if displayed, false otherwise
     */
    public boolean areJARAppsDisplayed() {
        return isElementDisplayed(getJARAppsTable());
    }

    /**
     * Checks if row for JAR with given name is displayed
     *
     * @param jarName -the name of the jar app expected
     * @return true if displayed, false otherwise
     */
    public boolean isJARDisplayed(String jarName) {
        return isElementDisplayed(getJARRow(jarName));
    }

    /**
     * Checks if row for JAR with given name is displayed
     *
     * @param jar -the jar app expected
     * @return true if displayed, false otherwise
     */
    public boolean isJARDisplayed(UserJarApp jar) {
        String jarName = jar.getName();
        return isJARDisplayed(jarName);
    }

    /**
     * Checks if row for app with given name is displayed
     *
     * @param appName -the name of the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(String appName) {
        return isElementDisplayed(getAppInJARRow(appName));
    }

    /**
     * Checks if row for given app is displayed
     *
     * @param app -the User JAR App expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayed(UserJarApp app) {
        String appName = app.getName();
        return isAppDisplayed(appName);
    }

    /**
     * Checks to see if the given app name is displayed in the table
     *
     * @param appName -name of the app expected
     * @return true if displayed, false if not
     */
    public boolean isAppNameDisplayed(String appName) {
        return isElementDisplayed(getJARAppNameCell(appName));
    }

    /**
     * Checks if app in JAR row is selected
     *
     * @param appName -the name of the app in the JAR to check
     * @return true if row selected, false otherwise
     */
    public boolean isAppInJARRowSelected(String appName) {
        El row = getAppInJARRow(appName).el(By.xpath("./ancestor::table"));
        String selected = row.getAttribute("class");
        return selected.contains("x-grid-item-selected");
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
     * @return the Create JAR Application Modal
     */
    public CreateJARApplicationModal clickCreate() {
        getCreateBtn().click();
        return getPage(CreateJARApplicationModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the Edit JAR Application Modal
     */
    public EditJARApplicationModal clickEdit() {
        getEditBtn().click();
        return getPage(EditJARApplicationModal.class);
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
     * Expands or collapses the given JAR based on the given boolean value
     *
     * @param jar -the JAR to expand
     * @param expand -true if JAR should be expanded, false if JAR should be
     *        collapsed
     */
    public void expandCollapseJAR(UserJarApp jar, boolean expand) {
        El icon = getExpandCollapseIcon(jar);
        String status = icon.getAttribute("data-qtip");
        if (expand) {
            Assert.assertTrue("The expand icon is not displayed as expected.", status.contains("expand"));
            icon.click();
        }
        else {
            Assert.assertTrue("The collapse icon is not displayed as expected.", status.contains("collapse"));
        }
    }

    /**
     * Selects given app name based on given boolean value
     *
     * @param appName -the name of the app to select
     * @param selected -true if app should be selected, false if not
     */
    public void selectAppInJAR(String appName, boolean selected) {
        El appRow = getAppInJARRow(appName);
        boolean isSelected = appRow.isSelected();
        if (selected && !isSelected) {
            appRow.click();
        }
    }

    /**
     * Selects given app based on given boolean value
     *
     * @param app -the user JAR app to select
     * @param checked -true if app should be selected, false if not
     */
    public void selectAppInJAR(UserJarApp app, boolean checked) {
        String appName = app.getName();
        selectAppInJAR(appName, checked);
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
