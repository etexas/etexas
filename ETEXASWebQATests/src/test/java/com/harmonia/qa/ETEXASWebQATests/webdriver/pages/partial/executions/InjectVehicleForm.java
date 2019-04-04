package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.VehicleInjectionCommand;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ExecutionsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial-page class representing the Inject Vehicle Form window
 *
 * @author llaroussini
 */
public class InjectVehicleForm extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public InjectVehicleForm(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the inject vehicle form header
     */
    private static final String INJECT_VEHICLE_HEADER_TEXT = "Inject Vehicle into Execution";

    /**
     * Text displayed in the inject vehicle help header
     */
    private static final String INJECT_VEHICLE_HELP_HEADER_TEXT = "Inject Vehicle Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "This command window allows the user to inject a vehicle into the simulation.  The user provides the lane number and speed of the vehicle to inject into an execution.";

    /**
     * The xpath of the lane dropdown
     */
    private static final String LANE_DROPDOWN_XPATH = "//input[@name='laneId']/ancestor::div[contains(@class, 'x-form-trigger-wrap-default')]";

    /**
     * The xpath of the lane dropdown selector
     */
    private static final String LANE_DROPDOWN_SELECTOR_XPATH = LANE_DROPDOWN_XPATH + "//div[contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * The xpath for dropdown options
     */
    private static final String DROPDOWN_OPTIONS_XPATH = "//div[contains(@class, 'x-boundlist-default')]//li[@role='option']";

    /**
     * The xpath prefix for a specific lane dropdown option
     */
    private static final String LANE_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX = DROPDOWN_OPTIONS_XPATH + "[text()='";

    /**
     * Xpath of the selected option in lane drop down
     */
    private static final String SELECTED_LANE_XPATH = "//li[contains(@class, 'x-boundlist-selected')]";

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
     * Label for the Lane field
     */
    private static final String LANE_FIELD_LABEL = "Lane";

    /**
     * Label for the Speed field
     */
    private static final String SPEED_FIELD_LABEL = "Speed";

    ///////////
    // Getters
    //////////

    /**
     * Gets the lane drop down menu
     *
     * @return the lane drop down
     */
    private El getLaneDropdown() {
        return el(By.xpath(LANE_DROPDOWN_XPATH));
    }

    /**
     * Gets the lane drop down selector
     *
     * @return the lane drop down selector
     */
    private El getLaneDropdownSelector() {
        return el(By.xpath(LANE_DROPDOWN_SELECTOR_XPATH));
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
    private El getInjectVehicleOKBtn() {
        return getFormBtn(INJECT_VEHICLE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(INJECT_VEHICLE_HEADER_TEXT, RESET_BTN_NAME);
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(INJECT_VEHICLE_HEADER_TEXT, CANCEL_BTN_NAME);
    }

    /**
     * The xpath to get from the inject vehicle header to the help icon
     *
     * @return the help icon
     */
    private El getInjectVehicleHelpIcon() {
        return getHelpIcon(INJECT_VEHICLE_HEADER_TEXT);
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
     * Gets the option selected in the Lane drop down
     *
     * @return the option element
     */
    private El getSelectedLane() {
        return el(By.xpath(SELECTED_LANE_XPATH));
    }

    /**
     * Gets the value selected in the lane drop down; first checks if a selected
     * lane can be found - if not drop down is closed and null is returned.
     * NOTE: This method also clicking the lane drop down to expand and then
     * clicks to close
     *
     * @return the displayed lane value
     */
    public String getDisplayedLane() {
        clickLaneDropdown();
        El lane = getSelectedLane();
        if (lane.exists()) {
            String laneSelected = lane.getText();
            clickLaneDropdown();
            return laneSelected;
        }
        else {
            clickLaneDropdown();
            return null;
        }

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
     * Gets the list size of the displayed lane option list
     *
     * @return the list size
     */
    public int getLaneOptionListSize() {
        return getDisplayedOptionList().size();
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the inject vehicle form header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isInjectVehicleHeaderDisplayed() {
        return isHeaderDisplayed(INJECT_VEHICLE_HEADER_TEXT);
    }

    /**
     * Checks to see if the inject vehicle help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isInjectVehicleHelpHeaderDisplayed() {
        return isHeaderDisplayed(INJECT_VEHICLE_HELP_HEADER_TEXT);

    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isInjectVehicleHelpIconDisplayed() {
        return isHelpIconDisplayed(INJECT_VEHICLE_HEADER_TEXT);
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isInjectVehicleCloseIconDisplayed() {
        return isCloseIconDisplayed(INJECT_VEHICLE_HEADER_TEXT);
    }

    /**
     * Checks if the help content is displayed
     *
     * @return true if displayed, false if not
     */
    public boolean isInjectVehicleHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the lane drop down is displayed
     *
     * @return true if the lane drop down is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isLaneDropdownDisplayed() {
        return isElementDisplayed(getLaneDropdown());
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
    public boolean isInjectVehicleOKBtnDisplayed() {
        return isBtnDisplayed(INJECT_VEHICLE_HEADER_TEXT, OK_BTN_NAME);
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isBtnDisplayed(INJECT_VEHICLE_HEADER_TEXT, RESET_BTN_NAME);
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isBtnDisplayed(INJECT_VEHICLE_HEADER_TEXT, CANCEL_BTN_NAME);
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
        return isHelpOKBtnDisplayed(INJECT_VEHICLE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if Invalid Number Speed error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidNumberSpeedErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(SPEED_FIELD_LABEL);
    }

    /**
     * Checks to see if Minimum Number Speed error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMinimumNumberSpeedErrorDisplayed() {
        return isMinimumNumberErrorDisplayed(SPEED_FIELD_LABEL);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Clicks the lane dropdown
     */
    public void clickLaneDropdown() {
        getLaneDropdownSelector().click();
    }

    /**
     * Click the OK button to save the inject vehicle command and waits for
     * window to close
     *
     * @return the newly loaded ExecutionsPage
     */
    public ExecutionsPage clickInjectVehicleOK() {
        getInjectVehicleOKBtn().click();
        waitForElementToBeInvisible(By.xpath(LANE_DROPDOWN_SELECTOR_XPATH));
        return getPage(ExecutionsPage.class);
    }

    /**
     * Click the OK button to save the inject vehicle command. Does not wait for
     * the element to close, used for error testing.
     *
     * @return the newly loaded ExecutionsPage
     */
    public ExecutionsPage clickInjectVehicleOKErrorExpected() {
        getInjectVehicleOKBtn().click();
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
     * Click the Inject Vehicle Help icon
     */
    public void clickInjectVehicleHelpIcon() {
        getInjectVehicleHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(INJECT_VEHICLE_HELP_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkInjectVehicleHeaderIcons() {
        checkHeaderIcons(INJECT_VEHICLE_HEADER_TEXT);
    }

    /**
     * Checks for the presence of the Lane dropdown and Speed text box
     */
    public void checkFields() {
        Assert.assertTrue("The Lane drop down could not be found.", isLaneDropdownDisplayed());
        Assert.assertTrue("The Speed text box could not be found.", isSpeedTextBoxDisplayed());
    }

    /**
     * Checks for the presence of the OK, Reset, and Cancel buttons in Lane
     * Change window
     */
    public void checkBtns() {
        Assert.assertTrue("The OK button could not be found.", isInjectVehicleOKBtnDisplayed());
        Assert.assertTrue("The Reset button could not be found.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button could not be found.", isCancelBtnDisplayed());
    }

    /**
     * Clicks the Lane drop down and selects the given lane
     *
     * @param lane -the lane to select
     */
    public void selectLane(String lane) {
        clickLaneDropdown();
        El option = el(By.xpath(LANE_DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX + lane + "']"));
        option.click();
    }

    /**
     * Clicks the Lane drop down and selects the lane from the given vehicle
     * injection command
     *
     * @param command -the vehicle injection command from which to get the lane
     */
    public void selectLane(VehicleInjectionCommand command) {
        String lane = command.getLane();
        selectLane(lane);
    }

    /**
     * Sets the given speed value in the Speed text box
     *
     * @param speed -the speed of the vehicle to set
     */
    public void setSpeed(String speed) {
        getSpeedTextBox().setText(speed);
    }

    /**
     * Sets the speed value of the given Vehicle Injection command in the Speed
     * text box
     *
     * @param command -the vehicle injection command from which to get the speed
     */
    public void setSpeed(VehicleInjectionCommand command) {
        String speed = command.getSpeed();
        setSpeed(speed);
    }

    /**
     * Checks that displayed values match given values
     *
     * @param lane -the lane expected to be selected
     * @param speed -the speed expected to be displayed
     */
    public void checkDisplayedSetFields(String lane, String speed) {
        Assert.assertEquals("Displayed lane does not match expected lane.", lane, getDisplayedLane());
        Assert.assertEquals("Displayed speed does not match expected speed.", speed, getDisplayedSpeed());
    }

    /**
     * Verifies the number of lanes listed in the lane drop down list matches
     * the expected number
     *
     * @param expectedSize -the expected number of lanes to be listed
     */
    public void checkLaneListSize(int expectedSize) {
        Assert.assertEquals("The number of lanes listed in the Lane drop down does not match the expected value.", expectedSize, getLaneOptionListSize());
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Lane and
     * Speed fields)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Lane field.", isFieldRequiredErrorDisplayed(LANE_FIELD_LABEL));
        Assert.assertTrue("Required field error is not displayed for the Speed field.", isFieldRequiredErrorDisplayed(SPEED_FIELD_LABEL));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(LANE_DROPDOWN_SELECTOR_XPATH));
    }

}