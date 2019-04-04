package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.SignalChangeCommand;
import com.harmonia.qa.ETEXASWebQATests.enums.SignalChangeCommandType;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ExecutionsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial-page class representing the Signal Change Form window
 *
 * @author llaroussini
 */
public class SignalChangeForm extends BaseForm {

    /**
     * Default Constructor
     *
     * @param driver -the web driver
     */
    public SignalChangeForm(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the signal change form header
     */
    private static final String SIGNAL_CHANGE_HEADER_TEXT = "Inject Signal Change into Execution";

    /**
     * Text displayed in the signal change help header
     */
    private static final String SIGNAL_CHANGE_HELP_HEADER_TEXT = "Inject Signal Change Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "The Inject Signal Change command allows the user to select a lane and choose a command to affect the signal status at a given time.";

    /**
     * The xpath of the time text box
     */
    private static final String TIME_TEXT_BOX_XPATH = "//input[@name='changeTime']";

    /**
     * The xpath of the command dropdown
     */
    private static final String COMMAND_DROPDOWN_XPATH = "//input[@name='command']/ancestor::div[contains(@class, 'x-form-trigger-wrap-default')]";

    /**
     * The xpath of the command dropdown selector
     */
    private static final String COMMAND_DROPDOWN_SELECTOR_XPATH = COMMAND_DROPDOWN_XPATH + "//div[contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * The xpath for dropdown options
     */
    private static final String DROPDOWN_OPTIONS_XPATH = "//div[contains(@class, 'x-boundlist-default')]//li[@role='option']";

    /**
     * The xpath prefix for a specific command dropdown option
     */
    private static final String COMMAND_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX = DROPDOWN_OPTIONS_XPATH + "[text()='";

    /**
     * Name of the OK button
     */
    private static final String OK_BTN_NAME = "OK";

    /**
     * Name of the Reset button
     */
    private static final String RESET_BTN_NAME = "Reset";

    /**
     * Name of the Cancel button
     */
    private static final String CANCEL_BTN_NAME = "Cancel";

    /**
     * Xpath of the selected option in drop down
     */
    private static final String SELECTED_OPTION_XPATH_PREFIX = ".//li[contains(@class, 'x-boundlist-selected')][text()='";

    /**
     * Label for the Time field
     */
    private static final String TIME_FIELD_LABEL = "Time";

    /**
     * Label for the Command field
     */
    private static final String COMMAND_FIELD_LABEL = "Command";

    ///////////
    // Getters
    //////////

    /**
     * Gets the time text box
     *
     * @return the time text box element
     */
    private El getTimeTextBox() {
        return el(By.xpath(TIME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the command drop down menu
     *
     * @return the command drop down
     */
    private El getCommandDropdown() {
        return el(By.xpath(COMMAND_DROPDOWN_XPATH));
    }

    /**
     * Gets the command drop down selector
     *
     * @return the command drop down selector
     */
    private El getCommandSelector() {
        return el(By.xpath(COMMAND_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the OK button
     *
     * @return the OK button
     */
    private El getSignalChangeOKBtn() {
        return getFormBtn(SIGNAL_CHANGE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(SIGNAL_CHANGE_HEADER_TEXT, RESET_BTN_NAME);
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(SIGNAL_CHANGE_HEADER_TEXT, CANCEL_BTN_NAME);
    }

    /**
     * The xpath to get from the signal change header to the help icon
     *
     * @return the help icon
     */
    private El getSignalChangeHelpIcon() {
        return getHelpIcon(SIGNAL_CHANGE_HEADER_TEXT);
    }

    /**
     * Gets the list of displayed options
     *
     * @return the list of options
     */
    private List<El> getDisplayedOptionList() {
        return els(By.xpath(DROPDOWN_OPTIONS_XPATH));
    }

    /**
     * Gets the value displayed in the time text box
     *
     * @return the displayed time value
     */
    public String getDisplayedTime() {
        return getTimeTextBox().getAttribute("value");
    }

    /**
     * Gets the selected option within drop down
     *
     * @param option -the option selected
     * @return the selected option element
     */
    private El getSelectedOption(String option) {
        return el(By.xpath(SELECTED_OPTION_XPATH_PREFIX + option + "']"));
    }

    /**
     * Gets the list size of the displayed vehicle option list
     *
     * @return the list size
     */
    public String getVehicleOptionListSize() {
        int listSize = getDisplayedOptionList().size();
        return Integer.toString(listSize);
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the signal change form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isSignalChangeHeaderDisplayed() {
        return isHeaderDisplayed(SIGNAL_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks to see if the signal change help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isSignalChangeHelpHeaderDisplayed() {
        return isHeaderDisplayed(SIGNAL_CHANGE_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSignalChangeHelpIconDisplayed() {
        return isHelpIconDisplayed(SIGNAL_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSignalChangeCloseIconDisplayed() {
        return isCloseIconDisplayed(SIGNAL_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks if the help content is displayed
     *
     * @return true if displayed, false if not
     */
    public boolean isSignalChangeHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the time text box is displayed
     *
     * @return true if the time text box is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isTimeTextBoxDisplayed() {
        return isElementDisplayed(getTimeTextBox());
    }

    /**
     * Checks to see if the command drop down is displayed
     *
     * @return true if the command drop down is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCommandDropdownDisplayed() {
        return isElementDisplayed(getCommandDropdown());
    }

    /**
     * Checks to see if the OK button is displayed
     *
     * @return true if the OK button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSignalChangeOKBtnDisplayed() {
        return isBtnDisplayed(SIGNAL_CHANGE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isBtnDisplayed(SIGNAL_CHANGE_HEADER_TEXT, RESET_BTN_NAME);

    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isBtnDisplayed(SIGNAL_CHANGE_HEADER_TEXT, CANCEL_BTN_NAME);

    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(SIGNAL_CHANGE_HELP_HEADER_TEXT);
    }

    /**
     * Checks if given command is selected -- method clicks command dropdown,
     * checks for presence of selected command, then closes dropdown (or returns
     * null and closes dropdown)
     *
     * @param command -the command expected to be selected
     * @return true if selected; false otherwise
     */
    public boolean isCommandSelected(String command) {
        clickCommandDropdown();
        try {
            El selectedCommand = getSelectedOption(command);
            clickCommandDropdown();
            return selectedCommand != null;
        }
        catch (NoSuchElementException e) {
            clickCommandDropdown();
            return false;
        }
    }

    /**
     * Checks to see if Invalid Number Time error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidNumberTimeErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(TIME_FIELD_LABEL);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Enters given text in time text box
     *
     * @param time -the time (as string) to enter
     */
    public void setTime(String time) {
        getTimeTextBox().setText(time);
    }

    /**
     * Enters time associated with given command
     *
     * @param command -the command for which the time should be set
     */
    public void setTime(SignalChangeCommand command) {
        String time = command.getTime();
        setTime(time);
    }

    /**
     * Clicks the Command dropdown
     */
    public void clickCommandDropdown() {
        getCommandSelector().click();
    }

    /**
     * Click the OK button to save the lane change command
     *
     * @return the newly loaded ExecutionsPage
     */
    public ExecutionsPage clickSignalChangeOK() {
        getSignalChangeOKBtn().click();
        return getPage(ExecutionsPage.class);
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getResetBtn().click();
        waitUntilLoaded();
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded/updated executions page
     */
    public ExecutionsPage clickCancel() {
        getCancelBtn().click();
        return getPage(ExecutionsPage.class);
    }

    /**
     * Click the Signal Change Help icon
     */
    public void clickSignalChangeHelpIcon() {
        getSignalChangeHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(SIGNAL_CHANGE_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkSignalChangeHeaderIcons() {
        checkHeaderIcons(SIGNAL_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Time text box and Command drop downs
     */
    public void checkFields() {
        Assert.assertTrue("The Time text box is not displayed.", isTimeTextBoxDisplayed());
        Assert.assertTrue("The Command drop down is not displayed.", isCommandDropdownDisplayed());
    }

    /**
     * Checks for the presence of the OK, Reset, and Cancel buttons in Lane
     * Change window
     */
    public void checkBtns() {
        Assert.assertTrue("The OK button is not displayed.", isSignalChangeOKBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Clicks the Command drop down and selects the given signal change command
     * type
     *
     * @param signalChangeCommandType -the signal change command type to select
     */
    public void selectSignalChangeCommandType(String signalChangeCommandType) {
        clickCommandDropdown();
        El templateOption = el(By.xpath(COMMAND_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX + signalChangeCommandType + "']"));
        templateOption.click();
    }

    /**
     * Sets the Time and Signal Change Command Type for the given command
     *
     * @param command -the signal change command to set
     */
    public void setSignalCommandInfo(SignalChangeCommand command) {
        setTime(command);
        selectSignalChangeCommandType(command);
    }

    /**
     * Clicks the Command drop down and selects the signal change command type
     *
     * @param command -the signal change command for which the signal change
     *        command type should be selected
     */
    public void selectSignalChangeCommandType(SignalChangeCommand command) {
        String signalChangeCommandType = command.getSignalChangeCommandType().getLabel();
        selectSignalChangeCommandType(signalChangeCommandType);
    }

    /**
     * Checks that displayed values match values for given command
     *
     * @param command -the signal change command expected
     */
    public void checkDisplayedSetFields(SignalChangeCommand command) {
        String id = command.getTime();
        String signalChangeCommandType = command.getSignalChangeCommandType().getLabel();
        Assert.assertEquals("Displayed time does not match expected time.", id, getDisplayedTime());
        Assert.assertTrue("Command, '" + signalChangeCommandType + "', is not selected as expected.", isCommandSelected(signalChangeCommandType));
    }

    /**
     * Checks that displayed values are cleared
     */
    public void checkClearedFields() {
        Assert.assertEquals("Time text box not cleared as exepcted.", "", getDisplayedTime());
        Assert.assertTrue("Command drop down was not cleared as expected.", isCommandSelected(null));
    }

    /**
     * Checks all listed command types in command drop down and verifies all
     * lane change command options are listed (Change Signal and Hold Signal)
     */
    public void checkSignalCommandList() {
        List<El> commands = getDisplayedOptionList();
        List<String> commandNames = new ArrayList<String>(4);
        for (El command : commands) {
            String name = command.getText();
            commandNames.add(name);
        }
        Assert.assertTrue("The command, Change Signal, could not be found in the Command drop down.", commandNames.contains(SignalChangeCommandType.CHANGE_SIGNAL.getLabel()));
        Assert.assertTrue("The command, Hold Signal, could not be found in the Command drop down.", commandNames.contains(SignalChangeCommandType.HOLD_SIGNAL.getLabel()));

    }

    /**
     * Verifies 'required field' error displays for all fields (checks Command
     * and Time fields)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Command field.", isFieldRequiredErrorDisplayed(COMMAND_FIELD_LABEL));
        Assert.assertTrue("Required field error is not displayed for the Time field.", isFieldRequiredErrorDisplayed(TIME_FIELD_LABEL));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TIME_TEXT_BOX_XPATH));
    }

}
