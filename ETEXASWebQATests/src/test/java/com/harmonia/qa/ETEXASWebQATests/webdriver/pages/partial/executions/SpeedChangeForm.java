package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.SpeedCommand;
import com.harmonia.qa.ETEXASWebQATests.enums.SpeedChangeCommand;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ExecutionsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the speed change form displayed when Speed Change is
 * selected from Commands button
 *
 * @author llaroussini
 */
public class SpeedChangeForm extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public SpeedChangeForm(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the speed change form header
     */
    private static final String SPEED_CHANGE_HEADER_TEXT = "Inject Speed Change into Execution";

    /**
     * Text displayed in the speed change help header
     */
    private static final String SPEED_CHANGE_HELP_HEADER_TEXT = "Inject Speed Change Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "The Inject Speed Change command allows the user to select a vehicle currently in the execution and cause it to change speed to the user entered speed and selected method of speed change.";

    /**
     * The xpath of the vehicle ID dropdown
     */
    private static final String VEHICLE_ID_DROPDOWN_XPATH = "//input[@name='vehicle']/ancestor::div[contains(@class, 'x-form-trigger-wrap-default')]";

    /**
     * The xpath of the vehicle ID dropdown selector
     */
    private static final String VEHICLE_ID_DROPDOWN_SELECTOR_XPATH = VEHICLE_ID_DROPDOWN_XPATH + "//div[contains(@class,'x-form-arrow-trigger-default')]";

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
     * The xpath prefix for a specific vehicle ID dropdown option
     */
    private static final String VEHICLE_ID_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX = DROPDOWN_OPTIONS_XPATH + "[text()='";

    /**
     * The xpath prefix for a specific command dropdown option
     */
    private static final String COMMAND_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX = DROPDOWN_OPTIONS_XPATH + "[text()='";

    /**
     * The xpath of the speed text box
     */
    private static final String SPEED_TEXT_BOX_XPATH = "//input[@name='speed']";

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
    private static final String SELECTED_OPTION_XPATH_PREFIX = "//li[contains(@class, 'x-boundlist-selected')][text()='";

    /**
     * Label for the Vehicle ID field
     */
    private static final String VEHICLE_ID_FIELD_LABEL = "Vehicle ID";

    /**
     * Label for the Command field
     */
    private static final String COMMAND_FIELD_LABEL = "Command";

    /**
     * Label for the Speed field
     */
    private static final String SPEED_FIELD_LABEL = "Speed";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the speed change header to the help icon
     *
     * @return the help icon
     */
    private El getSpeedChangeHelpIcon() {
        return getHelpIcon(SPEED_CHANGE_HEADER_TEXT);
    }

    /**
     * Gets the vehicle ID drop down menu
     *
     * @return the vehicle ID drop down
     */
    private El getVehicleIdDropdown() {
        return el(By.xpath(VEHICLE_ID_DROPDOWN_XPATH));
    }

    /**
     * Gets the vehicle ID drop down selector
     *
     * @return the vehicle ID drop down selector
     */
    private El getVehicleIdSelector() {
        return el(By.xpath(VEHICLE_ID_DROPDOWN_SELECTOR_XPATH));
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
     * Gets the speed text box
     *
     * @return the speed text box
     */
    private El getSpeedTextBox() {
        return el(By.xpath(SPEED_TEXT_BOX_XPATH));
    }

    /**
     * Gets the OK button
     *
     * @return the OK button
     */
    private El getSpeedChangeOKBtn() {
        return getFormBtn(SPEED_CHANGE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(SPEED_CHANGE_HEADER_TEXT, RESET_BTN_NAME);
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(SPEED_CHANGE_HEADER_TEXT, CANCEL_BTN_NAME);
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
     * Gets the selected option within drop down
     *
     * @param option -the option selected
     * @return the selected option element
     */
    private El getSelectedOption(String option) {
        return el(By.xpath(SELECTED_OPTION_XPATH_PREFIX + option + "']"));
    }

    /**
     * Gets the value in the speed text box
     *
     * @return the displayed Speed value
     */
    public String getDisplayedSpeed() {
        return getSpeedTextBox().getAttribute("value");
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
     * Checks to see if the speed change form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isSpeedChangeHeaderDisplayed() {
        return isHeaderDisplayed(SPEED_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks to see if the speed change help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isSpeedChangeHelpHeaderDisplayed() {
        return isHeaderDisplayed(SPEED_CHANGE_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSpeedChangeHelpIconDisplayed() {
        return isHelpIconDisplayed(SPEED_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSpeedChangeCloseIconDisplayed() {
        return isCloseIconDisplayed(SPEED_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks to see if the vehicle ID drop down is displayed
     *
     * @return true if the vehicle ID drop down is displayed, false if it is not
     *         or cannot be found
     */
    public boolean isVehicleIdDropdownDisplayed() {
        return isElementDisplayed(getVehicleIdDropdown());
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
     * Checks to see if the speed text box is displayed
     *
     * @return true if the speed text box is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isSpeedTextBoxDisplayed() {
        return isElementDisplayed(getSpeedTextBox());
    }

    /**
     * Checks to see if the OK button is displayed
     *
     * @return true if the OK button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSpeedChangeOKBtnDisplayed() {
        return isBtnDisplayed(SPEED_CHANGE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isBtnDisplayed(SPEED_CHANGE_HEADER_TEXT, RESET_BTN_NAME);

    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isBtnDisplayed(SPEED_CHANGE_HEADER_TEXT, CANCEL_BTN_NAME);

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
        return isHelpOKBtnDisplayed(SPEED_CHANGE_HELP_HEADER_TEXT);
    }

    /**
     * Checks if given vehicle ID is selected -- method clicks vehicle id
     * dropdown, checks for presence of selected id, then closes dropdown (or
     * returns null and closes dropdown)
     *
     * @param id -the vehicle ID expected to be selected
     * @return true if selected; false otherwise
     */
    public boolean isVehicleIDSelected(String id) {
        clickVehicleIdDropdown();
        try {
            El selectedVehicle = getSelectedOption(id);
            clickVehicleIdDropdown();
            return selectedVehicle != null;
        }
        catch (NoSuchElementException e) {
            clickVehicleIdDropdown();
            return false;
        }
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
     * Checks to see if Vehicle ID Field Required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isVehicleIDRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(VEHICLE_ID_FIELD_LABEL);
    }

    /**
     * Checks to see if Command Field Required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCommandRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(COMMAND_FIELD_LABEL);
    }

    /**
     * Checks to see if Speed Field Required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSpeedRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(SPEED_FIELD_LABEL);
    }

    /**
     * Checks to see if Invalid Number Speed error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidNumberSpeedErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(SPEED_FIELD_LABEL);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Clicks the Vehicle ID dropdown
     */
    public void clickVehicleIdDropdown() {
        getVehicleIdSelector().click();
    }

    /**
     * Clicks the Command dropdown
     */
    public void clickCommandDropdown() {
        getCommandSelector().click();
    }

    /**
     * Sets the speed value
     *
     * @param speed -the speed of the vehicle to set
     */
    public void setSpeed(String speed) {
        getSpeedTextBox().setText(speed);
    }

    /**
     * Sets the speed value
     *
     * @param command -the speed command for which the speed should be set
     */
    public void setSpeed(SpeedCommand command) {
        String speed = command.getSpeed();
        setSpeed(speed);
    }

    /**
     * Click the OK button to save the speed change command
     *
     * @return the newly loaded ExecutionsPage
     */
    public ExecutionsPage clickSpeedChangeOK() {
        getSpeedChangeOKBtn().click();
        return getPage(ExecutionsPage.class);
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getResetBtn().click();
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
     * Click the Speed Change Help icon
     */
    public void clickSpeedChangeHelpIcon() {
        getSpeedChangeHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(SPEED_CHANGE_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkSpeedChangeHeaderIcons() {
        checkHeaderIcons(SPEED_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Vehicle ID drop down, Command drop down,
     * and Speed text box
     */
    public void checkFields() {
        Assert.assertTrue("The Vehicle ID drop down is not displayed.", isVehicleIdDropdownDisplayed());
        Assert.assertTrue("The Command drop down is not displayed.", isCommandDropdownDisplayed());
        Assert.assertTrue("The Speed text box is not displayed.", isSpeedTextBoxDisplayed());
    }

    /**
     * Checks for the presence of the OK, Reset, and Cancel buttons in Speed
     * Change window
     */
    public void checkBtns() {
        Assert.assertTrue("The OK button is not displayed.", isSpeedChangeOKBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Clicks the Vehicle ID drop down and selects the vehicle id
     *
     * @param id -the id to select
     */
    public void selectVehicleId(String id) {
        clickVehicleIdDropdown();
        El templateOption = el(By.xpath(VEHICLE_ID_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX + id + "']"));
        templateOption.click();
    }

    /**
     * Clicks the Vehicle ID drop down and selects the vehicle id
     *
     * @param command -the speed command for which the vehicle id should be set
     */
    public void selectVehicleId(SpeedCommand command) {
        String id = command.getVehicleID();
        selectVehicleId(id);
    }

    /**
     * Clicks the Command drop down and selects the speed change command
     *
     * @param speedChangeCommand -the speed change command to select
     */
    public void selectSpeedChangeCommand(String speedChangeCommand) {
        clickCommandDropdown();
        El templateOption = el(By.xpath(COMMAND_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX + speedChangeCommand + "']"));
        templateOption.click();
    }

    /**
     * Sets the Vehicle ID, Speed Change Command, and Speed for the given
     * command
     *
     * @param command -the command to set
     */
    public void setCommandInfo(SpeedCommand command) {
        selectVehicleId(command);
        selectSpeedChangeCommand(command);
        setSpeed(command);
    }

    /**
     * Clicks the Command drop down and selects the speed change command
     *
     * @param command -the speed command for which the speed change command
     *        should be selected
     */
    public void selectSpeedChangeCommand(SpeedCommand command) {
        String speedChangeCommand = command.getSpeedChangeCommand().getLabel();
        selectSpeedChangeCommand(speedChangeCommand);
    }

    /**
     * Checks that displayed values match values for given command
     *
     * @param command -the speed command expected
     */
    public void checkDisplayedSetFields(SpeedCommand command) {
        String id = command.getVehicleID();
        String speedChangeCommand = command.getSpeedChangeCommand().getLabel();
        String speed = command.getSpeed();
        Assert.assertTrue("Vehicle with ID of " + id + " is not selected as expected.", isVehicleIDSelected(id));
        Assert.assertTrue("Command, '" + speedChangeCommand + "', is not selected as expected.", isCommandSelected(speedChangeCommand));
        Assert.assertEquals("Displayed speed does not match expected speed.", speed, getDisplayedSpeed());
    }

    /**
     * Checks that displayed values are cleared
     */
    public void checkClearedFields() {
        Assert.assertTrue("Vehicle ID drop down was not cleared as expected.", isVehicleIDSelected(null));
        Assert.assertTrue("Command drop down was not cleared as expected.", isCommandSelected(null));
        Assert.assertEquals("Speed text box was not cleared as expected.", "", getDisplayedSpeed());
    }

    /**
     * Verifies the number of vehicles listed in the vehicle ID drop down list
     * matches the expected number
     *
     * @param expectedSize -the expected number of vehicles to be listed
     */
    public void checkVehicleListSize(String expectedSize) {
        Assert.assertEquals("The number of vehicles listed in the vehicle ID drop down does not match the expected value.", expectedSize, getVehicleOptionListSize());
    }

    /**
     * Checks all listed commands in command drop down and verifies all speed
     * change command options are listed (Normal Accelerate, Max Accelerate,
     * Normal Decelerate, Max Decelerate)
     */
    public void checkSpeedCommandList() {
        List<El> commands = getDisplayedOptionList();
        List<String> commandNames = new ArrayList<String>(4);
        for (El command : commands) {
            String name = command.getText();
            commandNames.add(name);
        }
        Assert.assertTrue("Normal Accelerate could not be found in the Command drop down.", commandNames.contains(SpeedChangeCommand.NORMAL_ACCELERATE.getLabel()));
        Assert.assertTrue("Max Accelerate could not be found in the Command drop down.", commandNames.contains(SpeedChangeCommand.MAX_ACCELERATE.getLabel()));
        Assert.assertTrue("Normal Decelerate could not be found in the Command drop down.", commandNames.contains(SpeedChangeCommand.NORMAL_DECELERATE.getLabel()));
        Assert.assertTrue("Max Decelerate could not be found in the Command drop down.", commandNames.contains(SpeedChangeCommand.MAX_DECELERATE.getLabel()));
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Vehicle
     * ID, Command, and Speed fields)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Vehicle ID field.", isVehicleIDRequiredErrorDisplayed());
        Assert.assertTrue("Required field error is not displayed for the Command field.", isCommandRequiredErrorDisplayed());
        Assert.assertTrue("Required field error is not displayed for the Speed field.", isSpeedRequiredErrorDisplayed());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(VEHICLE_ID_DROPDOWN_SELECTOR_XPATH));
    }

}