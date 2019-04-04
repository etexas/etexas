package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing Edit A Native Application Modal
 *
 * @author llaroussini
 */
public class EditNativeApplicationModal extends CreateNativeApplicationModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public EditNativeApplicationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the Edit Native Application header
     */
    private static final String EDIT_NATIVE_APP_HEADER_TEXT = "Edit Native Application";

    /**
     * Xpath for Edit Native Application header
     */
    private static final String EDIT_NATIVE_APP_HEADER_XPATH = "//div[contains(@id, 'title')][text()='" + EDIT_NATIVE_APP_HEADER_TEXT + "']";

    /**
     * Text displayed in the Edit Native Application Help header
     */
    private static final String EDIT_NATIVE_APP_HELP_HEADER_TEXT = "Edit Native Application Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name, command line, host address, and port number for the selected native application. The application name must be unique across all applications.";

    /////////////
    // Getters
    /////////////

    /**
     * Gets the update button
     *
     * @return the update button
     */
    private El getUpdateBtn() {
        return getFormBtn(EDIT_NATIVE_APP_HEADER_TEXT, BtnNames.UPDATE.getLabel());
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(EDIT_NATIVE_APP_HEADER_TEXT, BtnNames.RESET.getLabel());
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(EDIT_NATIVE_APP_HEADER_TEXT, BtnNames.CANCEL.getLabel());
    }

    /**
     * Gets the edit native application help icon
     *
     * @return the help icon
     */
    private El getEditNativeAppHelpIcon() {
        return getHelpIcon(EDIT_NATIVE_APP_HEADER_TEXT);
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the edit native app form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditNativeAppHeaderDisplayed() {
        logSleepWait(5000);
        return isHeaderDisplayed(EDIT_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the edit native app help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isEditNativeAppHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_NATIVE_APP_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditNativeAppHelpIconDisplayed() {
        return isHelpIconDisplayed(EDIT_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isEditNativeAppCloseIconDisplayed() {
        return isCloseIconDisplayed(EDIT_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks to see if the update button is displayed
     *
     * @return true if the update button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isUpdateBtnDisplayed() {
        return isElementDisplayed(getUpdateBtn());
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    @Override
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    @Override
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_NATIVE_APP_HELP_HEADER_TEXT);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Click the Update button
     *
     * @return the newly loaded/updated Native Applications page
     */
    public NativeAppsPartialPage clickUpdate() {
        getUpdateBtn().click();
        return getPage(NativeAppsPartialPage.class);
    }

    /**
     * Click the Reset button
     */
    @Override
    public void clickReset() {
        getResetBtn().click();
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded/updated Native Applications page
     */
    @Override
    public NativeAppsPartialPage clickCancel() {
        getCancelBtn().click();
        return getPage(NativeAppsPartialPage.class);
    }

    /**
     * Click the Edit Native Application Help icon
     */
    public void clickEditNativeAppHelpIcon() {
        getEditNativeAppHelpIcon().click();
    }

    /**
     * Clicks the Close icon
     */
    @Override
    public void clickCloseIcon() {
        clickCloseIcon(EDIT_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Clicks the OK button in the help window
     */
    @Override
    public void clickHelpOKBtn() {
        clickHelpOKBtn(EDIT_NATIVE_APP_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkEditNativeAppHeaderIcons() {
        checkHeaderIcons(EDIT_NATIVE_APP_HEADER_TEXT);
    }

    /**
     * Checks for presence of both the Help header and content
     */
    public void checkHelpModal() {
        Assert.assertTrue("Edit Native Application Help header is not displayed as expected.", isEditNativeAppHelpHeaderDisplayed());
        Assert.assertTrue("Edit Native Application Help content is not displayed as expected.", isHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons in Edit
     * Native App window
     */
    @Override
    public void checkBtns() {
        Assert.assertTrue("The Update button is not displayed.", isUpdateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks for presence of all fields (Name text box, Device Type drop down,
     * Command Line text box, Host text box, and Port Number text box)
     */
    @Override
    public void checkAllFields() {
        Assert.assertTrue("Native App Name text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.NATIVE_APP_NAME));
        Assert.assertTrue("Command Line text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.CMD_LINE));
        Assert.assertTrue("Host text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.HOST));
        Assert.assertTrue("Port text box is not displayed.", isTextBoxDisplayed(NativeAppTextBox.PORT));
    }

    /**
     * Sets the given App Name, Command Line, Host, and Port fields
     *
     * @param appName - the app name (as string) to set
     * @param cmdLine - the command (as string) to set
     * @param host - the host (as string) to set
     * @param port - the port (as string) to set
     */
    public void setAllFields(String appName, String cmdLine, String host, String port) {
        setNativeAppName(appName);
        setCommandLine(cmdLine);
        setHost(host);
        setPort(port);
    }

    /**
     * Sets the Native App Name, Device Type, Command Line, Host, and Port
     * fields with info associated with the given app
     *
     * @param app -the User Native App who values should be set
     */
    @Override
    public void setAllFields(UserNativeApp app) {
        setNativeAppName(app);
        setCommandLine(app);
        setHost(app);
        setPort(app);
    }

    /**
     * Checks that displayed values match values associated with the given app
     *
     * @param app - User Native App expected
     */
    @Override
    public void checkAllFields(UserNativeApp app) {
        String name = app.getName();
        String cmdLine = app.getCommandLine();
        String host = app.getHost();
        String port = app.getPort();
        Assert.assertEquals("Native App Name value not displayed as expected.", name, getDisplayedAppName());
        Assert.assertEquals("Command Line value not displayed as expected.", cmdLine, getDisplayedCommandLine());
        Assert.assertEquals("Host value not displayed as expected.", host, getDisplayedHost());
        Assert.assertEquals("Port value not displayed as expected.", port, getDisplayedPort());
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(EDIT_NATIVE_APP_HEADER_XPATH), 10);
    }

}
