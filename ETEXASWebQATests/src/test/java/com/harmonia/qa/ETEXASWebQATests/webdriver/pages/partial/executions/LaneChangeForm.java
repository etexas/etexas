package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.LaneChangeCommand;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneChangeCommandType;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ExecutionsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial-page class representing the Lane Change Form window
 *
 * @author llaroussini
 */
public class LaneChangeForm extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public LaneChangeForm(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the lane change form header
     */
    private static final String LANE_CHANGE_HEADER_TEXT = "Inject Lane Change into Execution";

    /**
     * Text displayed in the lane change help header
     */
    private static final String LANE_CHANGE_HELP_HEADER_TEXT = "Inject Lane Change Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "The Lane Change Command allows the user select a vehicle currently in the execution and issue a command to cause the vehicle to change lanes in the direction selected.";

    /**
     * The xpath of the vehicle ID dropdown
     */
    private static final String VEHICLE_ID_DROPDOWN_XPATH = "//input[@name='vehicleId']/ancestor::div[contains(@class, 'x-form-trigger-wrap-default')]";

    /**
     * The xpath of the vehicle ID dropdown selector
     */
    private static final String VEHICLE_ID_DROPDOWN_SELECTOR_XPATH = VEHICLE_ID_DROPDOWN_XPATH + "//div[contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * The xpath of the command dropdown
     */
    private static final String COMMAND_DROPDOWN_XPATH = "//input[@name='changeLeft']/ancestor::div[contains(@class, 'x-form-trigger-wrap-default')]";

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
     * Label for the Vehicle ID field
     */
    private static final String VEHICLE_ID_FIELD_LABEL = "Vehicle ID";

    /**
     * Label for the Command field
     */
    private static final String COMMAND_FIELD_LABEL = "Command";

    ///////////
    // Getters
    //////////

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
     * Gets the OK button
     *
     * @return the OK button
     */
    private El getLaneChangeOKBtn() {
        return getFormBtn(LANE_CHANGE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(LANE_CHANGE_HEADER_TEXT, RESET_BTN_NAME);
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(LANE_CHANGE_HEADER_TEXT, CANCEL_BTN_NAME);
    }

    /**
     * The xpath to get from the speed change header to the help icon
     *
     * @return the help icon
     */
    private El getLaneChangeHelpIcon() {
        return getHelpIcon(LANE_CHANGE_HEADER_TEXT);
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
     * Checks to see if the lane change form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isLaneChangeHeaderDisplayed() {
        return isHeaderDisplayed(LANE_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks to see if the lane change help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isLaneChangeHelpHeaderDisplayed() {
        return isHeaderDisplayed(LANE_CHANGE_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isLaneChangeHelpIconDisplayed() {
        return isHelpIconDisplayed(LANE_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isLaneChangeCloseIconDisplayed() {
        return isCloseIconDisplayed(LANE_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks if the help content is displayed
     *
     * @return true if displayed, false if not
     */
    public boolean isLaneChangeHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
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
     * Checks to see if the OK button is displayed
     *
     * @return true if the OK button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isLaneChangeOKBtnDisplayed() {
        return isBtnDisplayed(LANE_CHANGE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isBtnDisplayed(LANE_CHANGE_HEADER_TEXT, RESET_BTN_NAME);

    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isBtnDisplayed(LANE_CHANGE_HEADER_TEXT, CANCEL_BTN_NAME);

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
        return isHelpOKBtnDisplayed(LANE_CHANGE_HELP_HEADER_TEXT);
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
     * Click the OK button to save the lane change command
     *
     * @return the newly loaded ExecutionsPage
     */
    public ExecutionsPage clickLaneChangeOK() {
        getLaneChangeOKBtn().click();
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
     * Click the Lane Change Help icon
     */
    public void clickLaneChangeHelpIcon() {
        getLaneChangeHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(LANE_CHANGE_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkLaneChangeHeaderIcons() {
        checkHeaderIcons(LANE_CHANGE_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Vehicle ID and Command drop downs
     */
    public void checkFields() {
        Assert.assertTrue("The Vehicle ID drop down is not displayed.", isVehicleIdDropdownDisplayed());
        Assert.assertTrue("The Command drop down is not displayed.", isCommandDropdownDisplayed());
    }

    /**
     * Checks for the presence of the OK, Reset, and Cancel buttons in Lane
     * Change window
     */
    public void checkBtns() {
        Assert.assertTrue("The OK button is not displayed.", isLaneChangeOKBtnDisplayed());
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
     * @param command -the lane change command for which the vehicle id should
     *        be set
     */
    public void selectVehicleId(LaneChangeCommand command) {
        String id = command.getVehicleID();
        selectVehicleId(id);
    }

    /**
     * Clicks the Command drop down and selects the given lane change command
     * type
     *
     * @param laneChangeCommandType -the lane change command type to select
     */
    public void selectLaneChangeCommandType(String laneChangeCommandType) {
        clickCommandDropdown();
        El templateOption = el(By.xpath(COMMAND_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX + laneChangeCommandType + "']"));
        templateOption.click();
    }

    /**
     * Sets the Vehicle ID and Lane Change Command Type for the given command
     *
     * @param command -the lane change command to set
     */
    public void setCommandInfo(LaneChangeCommand command) {
        selectVehicleId(command);
        selectLaneChangeCommandType(command);
    }

    /**
     * Clicks the Command drop down and selects the lane change command type
     *
     * @param command -the lane change command for which the lane change command
     *        type should be selected
     */
    public void selectLaneChangeCommandType(LaneChangeCommand command) {
        String laneChangeCommandType = command.getLaneChangeCommandType().getLabel();
        selectLaneChangeCommandType(laneChangeCommandType);
    }

    /**
     * Checks that displayed values match values for given command
     *
     * @param command -the lane change command expected
     */
    public void checkDisplayedSetFields(LaneChangeCommand command) {
        String id = command.getVehicleID();
        String laneChangeCommandType = command.getLaneChangeCommandType().getLabel();
        Assert.assertTrue("Vehicle with ID of " + id + " is not selected as expected.", isVehicleIDSelected(id));
        Assert.assertTrue("Command, '" + laneChangeCommandType + "', is not selected as expected.", isCommandSelected(laneChangeCommandType));
    }

    /**
     * Checks that displayed values are cleared
     */
    public void checkClearedFields() {
        Assert.assertTrue("Vehicle ID drop down was not cleared as expected.", isVehicleIDSelected(null));
        Assert.assertTrue("Command drop down was not cleared as expected.", isCommandSelected(null));
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
     * Checks all listed command types in command drop down and verifies all
     * lane change command options are listed (Change Lane Left and Change Lane
     * Right)
     */
    public void checkLaneCommandList() {
        List<El> commands = getDisplayedOptionList();
        List<String> commandNames = new ArrayList<String>(4);
        for (El command : commands) {
            String name = command.getText();
            commandNames.add(name);
        }
        Assert.assertTrue("The command, Change Lane Left, could not be found in the Command drop down.", commandNames.contains(LaneChangeCommandType.CHANGE_LANE_LEFT.getLabel()));
        Assert.assertTrue("The command, Change Lane Right, could not be found in the Command drop down.", commandNames.contains(LaneChangeCommandType.CHANGE_LANE_RIGHT.getLabel()));

    }

    /**
     * Verifies 'required field' error displays for all fields (checks Vehicle
     * ID and Command fields)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Vehicle ID field.", isFieldRequiredErrorDisplayed(VEHICLE_ID_FIELD_LABEL));
        Assert.assertTrue("Required field error is not displayed for the Command field.", isFieldRequiredErrorDisplayed(COMMAND_FIELD_LABEL));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(VEHICLE_ID_DROPDOWN_SELECTOR_XPATH));
    }

}
