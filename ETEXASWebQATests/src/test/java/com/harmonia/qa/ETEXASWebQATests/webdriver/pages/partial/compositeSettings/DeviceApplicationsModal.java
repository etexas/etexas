package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;

import com.harmonia.qa.ETEXASWebQATests.entities.AppParameter;
import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EditParameterModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

import junit.framework.Assert;

/**
 * Page class representing the Applications Modal
 *
 * @author llaroussini
 */
public class DeviceApplicationsModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public DeviceApplicationsModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////
    //Enumerations
    //////////////

    /**
     * Header cells listed in the Parameters table
     *
     * @author llaroussini
     */
    public enum ParamColumnHeader {
        /**
         * The Name column header
         */
        NAME("Name"),

        /**
         * The Value column header
         */
        VALUE("Value");

        /**
         * The label of the column headers as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        ParamColumnHeader(String label) {
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
     * Text displayed in the device applications header
     */
    private static final String DEVICE_APPLICATIONS_HEADER_TEXT = "Device Applications";

    /**
     * The xpath for the close icon
     */
    private static final String CLOSE_ICON_XPATH = "//div[contains(@class, 'x-tool-close')]";

    /**
     * The xpath for the close button
     */
    private static final String CLOSE_BTN_XPATH = "//span[contains(@id, 'Executables')][contains(@id, 'close')]";

    /**
     * Text displayed in the device applications help header
     */
    private static final String DEVICE_APPLICATIONS_HELP_HEADER_TEXT = "Device Applications Help";

    /**
     * Text displayed in the device applications help modal
     */
    private static final String DEVICE_APPLICATIONS_HELP_CONTENT_TEXT = "Select the applications that will be hosted on the relevant device(s) by moving them from the";

    /**
     * Xpath to the available applications section
     */
    private static final String AVAILABLE_APPS_SECTION_XPATH = "//div[contains(@id, 'available')][contains(@class, 'x-panel-default')]";

    /**
     * Xpath to the available applications list
     */
    private static final String AVAILABLE_APPS_LIST_XPATH = "//div[contains(@id, 'available')]//table";

    /**
     * Xpath prefix to available application row
     */
    private static final String AVAILABLE_APP_ROW_XPATH_PREFIX = AVAILABLE_APPS_LIST_XPATH + "//div[contains(text(), '";

    /**
     * Xpath to the hosted applications section
     */
    private static final String HOSTED_APPS_SECTION_XPATH = "//div[contains(@id, 'executable')][contains(@class, 'x-panel-default')]";

    /**
     * Xpath to the hosted applications list
     */
    private static final String HOSTED_APPS_LIST_XPATH = "//div[contains(@id, 'executable')]//table";

    /**
     * Xpath prefix to hosted application row
     */
    private static final String HOSTED_APP_ROW_XPATH_PREFIX = HOSTED_APPS_LIST_XPATH + "//div[contains(text(), '";

    /**
     * Xpath suffix to application row
     */
    private static final String APP_ROW_XPATH_SUFFIX = "')]/ancestor::tr";

    /**
     * Xpath to the Add button
     */
    private static final String ADD_BTN_XPATH = "//span[contains(@id, 'add')]/ancestor::a";

    /**
     * Xpath to the Remove button
     */
    private static final String REMOVE_BTN_XPATH = "//span[contains(@id, 'remove')]/ancestor::a";

    /**
     * XPath to the Parameters section
     */
    private static final String PARAMETERS_SECTION_XPATH = "//div[contains(@id,'parameters')][contains(@class, 'x-panel-default')]";

    /**
     * Xpath to the Edit button
     */
    private static final String EDIT_BTN_XPATH = "//span[contains(@id, 'parameter')][contains(@id, 'edit')]/ancestor::a";

    /**
     * XPath prefix to a specific App Parameter row
     */
    private static final String SPECIFIC_APP_PARAMETER_XPATH_PREFIX = PARAMETERS_SECTION_XPATH + "//table//div[contains(text(), '";

    /**
     * Xpath to parameter name cell, to be used with parameter row
     */
    private static final String PARAMETER_NAME_CELL_XPATH_SUFFIX = ".//td[contains(@class, 'parameter')][contains(@class, 'name')]/div";

    /**
     * Xpath to parameter value cell, to be used with parameter row
     */
    private static final String PARAMETER_VALUE_CELL_XPATH_SUFFIX = ".//td[contains(@class, 'parameter')][contains(@class, 'value')]/div";

    /**
     * XPath suffix to a specific App Parameter row
     */
    private static final String SPECIFIC_APP_PARAMETER_ROW_XPATH_SUFFIX = "')]/ancestor::tr";

    /**
     * Xpath suffix to a specific App Parameter value -- used to find a value
     * associated with a given parameter name
     */
    private static final String SPECIFIC_APP_PARAMETER_VALUE_XPATH_SUFFIX = SPECIFIC_APP_PARAMETER_ROW_XPATH_SUFFIX + "//td[contains(@class, 'value')]";

    /**
     * Xpath to the Parameters table
     */
    private static final String PARAMETERS_TABLE_XPATH = PARAMETERS_SECTION_XPATH + "//tr";

    /**
     * Xpath prefix to Parameters table column headers
     */
    private static final String PARAMETERS_TABLE_COLUMN_HEADER_XPATH_PREFIX = PARAMETERS_SECTION_XPATH + "//span[contains(@id, 'parameter-grid')][text()='";

    /**
     * By locator suitable for locating one or several app entries. Note that
     * this path is intended to function as a path FROM the section TO the
     * specific apps listed
     */
    private static final By APP_LOCATOR = By.xpath(".//table//div");

    ///////////
    // Getters
    ///////////

    /**
     * The xpath to get from the device applications header to the help icon
     *
     * @return the help icon
     */
    private El getHelpIcon() {
        return getHelpIcon(DEVICE_APPLICATIONS_HEADER_TEXT);
    }

    /**
     * Gets the close icon
     *
     * @return the close icon
     */
    private El getCloseIcon() {
        return el(By.xpath(CLOSE_ICON_XPATH));
    }

    /**
     * Gets the close button
     *
     * @return the close button
     */
    private El getCloseBtn() {
        return el(By.xpath(CLOSE_BTN_XPATH));
    }

    /**
     * Gets the Available section
     *
     * @return the section element
     */
    private El getAvailableSection() {
        return el(By.xpath(AVAILABLE_APPS_SECTION_XPATH));
    }

    /**
     * Gets the Hosted section
     *
     * @return the section element
     */
    private El getHostedSection() {
        return el(By.xpath(HOSTED_APPS_SECTION_XPATH));
    }

    /**
     * Gets the Add app button
     *
     * @return the add app (right-hand arrow) button
     */
    private El getAddAppBtn() {
        return el(By.xpath(ADD_BTN_XPATH));
    }

    /**
     * Gets the Remove app button
     *
     * @return the remove app (left-hand arrow) button
     */
    private El getRemoveAppBtn() {
        return el(By.xpath(REMOVE_BTN_XPATH));
    }

    /**
     * Gets the Parameters section
     *
     * @return the parameters section
     */
    private El getParametersSection() {
        return el(By.xpath(PARAMETERS_SECTION_XPATH));
    }

    /**
     * Gets the given column header cell in the Parameters table
     *
     * @param header -the header cell to get
     * @return the header cell element
     */
    private El getParametersColumnHeaderCell(ParamColumnHeader header) {
        return el(By.xpath(PARAMETERS_TABLE_COLUMN_HEADER_XPATH_PREFIX + header.getLabel() + "']"));
    }

    /**
     * Gets a specific Parameter
     *
     * @param paramName -the name of the parameter
     * @return the parameter
     */
    private El getParameter(String paramName) {
        return el(By.xpath(SPECIFIC_APP_PARAMETER_XPATH_PREFIX + paramName + "')]"));
    }

    /**
     * Gets the Edit button
     *
     * @return the Edit button
     */
    private El getEditBtn() {
        return el(By.xpath(EDIT_BTN_XPATH));
    }

    /**
     * Gets the row associated with a specific Application Parameter
     *
     * @param paramName -the name of the application parameter
     * @return the row element
     */
    private El getAppParamRow(String paramName) {
        return el(By.xpath(SPECIFIC_APP_PARAMETER_XPATH_PREFIX + paramName + SPECIFIC_APP_PARAMETER_ROW_XPATH_SUFFIX));
    }

    /**
     * Gets the value associated with a specific Parameter
     *
     * @param paramName -the name of the parameter
     * @return the param value (as string)
     */
    public String getAppParamValue(String paramName) {
        El valueArea = el(By.xpath(SPECIFIC_APP_PARAMETER_XPATH_PREFIX + paramName + SPECIFIC_APP_PARAMETER_VALUE_XPATH_SUFFIX));
        return valueArea.getText();
    }

    /**
     * Gets a map of params and their values as displayed in the parameters
     * section
     *
     * @return a map of params and their values displayed
     */
    public Map<String, String> getParamsAndValues() {
        waitUntilLoaded();
        Map<String, String> params = new HashMap<String, String>(0);
        El paramsSection = getParametersSection();
        List<El> paramRows = paramsSection.els(By.xpath(PARAMETERS_TABLE_XPATH));
        for (El row : paramRows) {
            El nameDiv = row.el(By.xpath(PARAMETER_NAME_CELL_XPATH_SUFFIX));
            El valueDiv = row.el(By.xpath(PARAMETER_VALUE_CELL_XPATH_SUFFIX));
            params.put(nameDiv.getText(), valueDiv.getText());
        }
        return params;
    }

    /**
     * Gets a set of strings representing parameters displayed in the parameters
     * section
     *
     * @return the set of available parameters
     */
    public Set<String> getAppParameters() {
        return getParamsAndValues().keySet();
    }

    /**
     * Gets a string list representing the applications displayed as available
     *
     * @return a list of string representations of the available applications
     */
    public List<String> getAvailableApps() {
        List<String> availableApps = new ArrayList<String>(0);
        El availableAppsSection = getAvailableSection();
        List<El> appEls = availableAppsSection.els(APP_LOCATOR);
        for (El el : appEls) {
            availableApps.add(el.getText());
        }
        return availableApps;
    }

    /**
     * Gets a string list representing the applications displayed as hosted
     *
     * @return a list of string representations of the hosted applications
     */
    public List<String> getHostedApps() {
        List<String> hostedApps = new ArrayList<String>(0);
        El hostedAppsSection = getHostedSection();
        List<El> appEls = hostedAppsSection.els(APP_LOCATOR);
        for (El el : appEls) {
            String text = el.getText();
            hostedApps.add(el.getText());
        }
        return hostedApps;
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the device applications header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isDeviceApplicationsHeaderDisplayed() {
        return isHeaderDisplayed(DEVICE_APPLICATIONS_HEADER_TEXT);
    }

    /**
     * Checks to see if the device applications help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isDeviceApplicationsHelpHeaderDisplayed() {
        return isHeaderDisplayed(DEVICE_APPLICATIONS_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isDeviceApplicationsHelpContentDisplayed() {
        return isContentDisplayed(DEVICE_APPLICATIONS_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isDeviceApplicationsHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(DEVICE_APPLICATIONS_HELP_HEADER_TEXT);
    }

    /**
     * Checks if the Help icon is displayed in the Device Applications modal
     *
     * @return true if displayed, false otherwise
     */
    public boolean isHelpIconDisplayed() {
        return isElementDisplayed(getHelpIcon());
    }

    /**
     * Checks if the Close icon is displayed in the Device Applications modal
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCloseIconDisplayed() {
        return isElementDisplayed(getCloseIcon());
    }

    /**
     * Checks to see if the close button is displayed
     *
     * @return true if the close button is displayed, false otherwise
     */
    public boolean isCloseBtnDisplayed() {
        return isElementDisplayed(getCloseBtn());
    }

    /**
     * Checks to see if edit button is displayed
     *
     * @return true if the edit button is displayed, false otherwise
     */
    public boolean isEditBtnDisplayed() {
        return isElementDisplayed(getEditBtn());
    }

    /**
     * Checks to see if edit button is enabled
     *
     * @return true if the edit button is enabled, false if disabled
     */
    public boolean isEditBtnEnabled() {
        return isBtnEnabled(getEditBtn());
    }

    /**
     * Checks to see if the available section is displayed
     *
     * @return true if the available section is displayed, false otherwise
     */
    public boolean isAvailableSectionDisplayed() {
        return isElementDisplayed(getAvailableSection());
    }

    /**
     * Checks to see if the hosted section is displayed
     *
     * @return true if the hosted section is displayed, false otherwise
     */
    public boolean isHostedSectionDisplayed() {
        return isElementDisplayed(getHostedSection());
    }

    /**
     * Checks to see if the add app button ('>') is displayed
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isAddAppBtnDisplayed() {
        return isElementDisplayed(getAddAppBtn());
    }

/**
     * Checks to see if the remove app button ('<') is displayed
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isRemoveAppBtnDisplayed() {
        return isElementDisplayed(getRemoveAppBtn());
    }

    /**
     * Checks to see if the parameters section is displayed
     *
     * @return true if the parameters section is displayed, false otherwise
     */
    public boolean isAppParametersSectionDisplayed() {
        return isElementDisplayed(getParametersSection());
    }

    /**
     * Checks if the given header cell is displayed in the Parameters table
     *
     * @param header -the header cell expected
     * @return true if displayed, false otherwise
     */
    public boolean isParameterTableHeaderCellDisplayed(ParamColumnHeader header) {
        return isElementDisplayed(getParametersColumnHeaderCell(header));
    }

    /**
     * Checks to see if the given parameter is displayed
     *
     * @param paramName -the name of the parameter expected
     * @return true if the parameter is displayed, false otherwise
     */
    public boolean isAppParameterDisplayed(String paramName) {
        return isElementDisplayed(getParameter(paramName));
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Click the Device Applications Help icon
     */
    public void clickDeviceApplicationsHelpIcon() {
        getHelpIcon().click();
    }

    /**
     * Clicks the Device Applications Close icon
     */
    public void clickCloseIcon() {
        getCloseIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickDeviceApplicationsHelpOKBtn() {
        clickHelpOKBtn(DEVICE_APPLICATIONS_HELP_HEADER_TEXT);
    }

    /**
     * Clicks the Close button
     */
    public void clickCloseBtn() {
        getCloseBtn().click();
    }

    /**
     * Selects the given parameter found in the table
     */
    public void selectParam(String paramName) {
        El paramCheckbox = getAppParamRow(paramName);
        paramCheckbox.click();
    }

    /**
     * Clicks an application with the specified name. Note that there is no
     * safety here to ensure the specified application is actually present
     *
     * @param appName the name of the application to select
     */
    public void selectAvailableApp(String appName) {
        waitUntilLoaded();
        El availableAppsSection = getAvailableSection();
        List<El> appEls = availableAppsSection.els(APP_LOCATOR);
        for (El el : appEls) {
            if (el.getText().contains(appName)) {
                el.click();
            }
        }
    }

    /**
     * Clicks the add app button
     */
    public void clickAddApp() {
        getAddAppBtn().click();
    }

    /**
     * Clicks the remove app button
     */
    public void clickRemoveApp() {
        getRemoveAppBtn().click();
    }

    /**
     * Selects an application with the given name from the list of hosted
     * applications
     *
     * @param appName the applications to select from the list of hosted
     *        applications
     */
    public void selectHostedApp(String appName) {
        waitUntilLoaded();
        El hostedAppsSection = getHostedSection();
        List<El> appEls = hostedAppsSection.els(APP_LOCATOR);
        for (El el : appEls) {
            if (el.getText().contains(appName)) {
                el.click();
                ETexasCommonUtils.sleep(2000); //ZZZ To ensure the app is selected
            }
        }
        waitForElementToBeVisible(By.xpath(PARAMETERS_TABLE_XPATH));
    }

    /**
     * Clicks the edit button
     *
     * @return the newly loaded Edit Parameter form
     */
    public EditParameterModal clickEdit() {
        getEditBtn().click();
        return getPage(EditParameterModal.class);
    }

    /**
     * Clicks the first parameter found in the table and waits for Edit button
     * to be clickable
     */
    public void selectFirstParam() {
        El paramsSection = getParametersSection();
        List<El> paramRows = paramsSection.els(By.xpath(PARAMETERS_TABLE_XPATH));
        paramRows.get(0).click();
        waitForExpectedCondition(ExpectedConditions.elementToBeClickable(getEditBtn()), true, PAGE_LOAD_TIMEOUT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Verifies the Device Applications header is displayed
     */
    public void checkHeader() {
        Assert.assertTrue("Device Applications header not displayed as expected.", isDeviceApplicationsHeaderDisplayed());
    }

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkHeaderIcons() {
        Assert.assertTrue("Help icon not displayed as expected in the Device Applications modal.", isHelpIconDisplayed(DEVICE_APPLICATIONS_HEADER_TEXT));
        Assert.assertTrue("Close icon not displayed as expected in the Device Applications modal.", isCloseIconDisplayed());
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Device Applications Help header not displayed as expected.", isDeviceApplicationsHelpHeaderDisplayed());
        Assert.assertTrue("Device Applications Help content not displayed as expected.", isDeviceApplicationsHelpContentDisplayed());
    }

    /**
     * Checks to see that the following sections are displayed: Available,
     * Hosted, and Parameters
     */
    public void checkAppSectionsDisplayed() {
        Assert.assertTrue("The Available section is not displayed in the Device Applications modal.", isAvailableSectionDisplayed());
        Assert.assertTrue("The Hosted section is not displayed in the Device Applications modal.", isHostedSectionDisplayed());
        Assert.assertTrue("The Parameters section is not displayed in the Device Applications modal.", isAppParametersSectionDisplayed());
    }

    /**
     * Checks to see if all expected header cells are displayed in Parameters
     * table (Name and Value)
     */
    public void checkParametersTableHeaderCells() {
        Assert.assertTrue("Name column header cell could not be found in Parameters table.", isParameterTableHeaderCellDisplayed(ParamColumnHeader.NAME));
        Assert.assertTrue("Value column header cell could not be found in Parameters table.", isParameterTableHeaderCellDisplayed(ParamColumnHeader.VALUE));
    }

    /**
     * Checks to see if a given application is listed as hosted
     *
     * @param appName the application being checked
     * @return true if an application with the given name is hosted, false
     *         otherwise
     */
    public boolean isAppHosted(String appName) {
        El installedAppsSection = getHostedSection();
        List<El> appEls = installedAppsSection.els(APP_LOCATOR);
        boolean installed = false;
        for (El el : appEls) {
            if (el.getText().contains(appName)) {
                installed = true;
                break;
            }
        }
        return installed;
    }

    /**
     * Checks to see if an application with the specified name is displayed in
     * the list of available applications
     *
     * @param name the name which is being checked in the list of available
     *        applications
     * @return true if an application with the specified name is found in the
     *         list of available applications
     */
    public boolean isAppAvailable(String name) {
        List<String> availableApps = getAvailableApps();
        boolean available = false;
        for (String app : availableApps) {
            if (app.contains(name)) {
                available = true;
                break;
            }
        }
        return available;
    }

    /**
     * Checks for presence of all param names associated with the given built in
     * app
     *
     * @param app -the app to check
     */
    public void checkAllParamNamesDisplayed(EmbeddedApp app) {
        for (AppParameter param : app.getParameters()) {
            String paramName = param.getParameterName();
            Assert.assertTrue("The parameter with the name: " + paramName + " could not be found.", isAppParameterDisplayed(paramName));
        }
    }

    /**
     * Checks for presence of all param values (based on the param names)
     * associated with the given built in app
     *
     * @param app -the app to check
     */
    public void checkAllParamValuesDisplayed(EmbeddedApp app) {
        for (AppParameter param : app.getParameters()) {
            String paramName = param.getParameterName();
            String value = param.getParameterValue();
            String displayedValue = getAppParamValue(paramName);
            Assert.assertEquals("The app parameter value for app: " + app.getName() + " associated with the parameter name: " + paramName + " could not be found.", value, displayedValue);
        }
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TITLE_XPATH_PREFIX + DEVICE_APPLICATIONS_HEADER_TEXT + "']"));
    }

    /**
     * Waits for list to be displayed in Installed Apps section
     */
    public void waitForHosterAppList() {
        waitForElementToBePresent(By.xpath(HOSTED_APPS_LIST_XPATH));
    }

    /**
     * Waits for the given app to be displayed in the Hosted App list
     *
     * @param appName -the name of the app
     */
    public void waitForSpecificHostedApp(String appName) {
        waitForElementToBeVisible(By.xpath(HOSTED_APP_ROW_XPATH_PREFIX + appName + APP_ROW_XPATH_SUFFIX));
    }

    /**
     * Waits for the given app to be displayed in the Hosted App list
     *
     * @param app -built in app
     */
    public void waitForSpecificHostedApp(EmbeddedApp app) {
        String appName = app.getName();
        waitForSpecificHostedApp(appName);
    }
}