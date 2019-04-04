package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.SelectDetectorLaneModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the add detectors form
 *
 * @author rsmith
 */
public class CreateDetectorModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public CreateDetectorModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////////
    // Enumerations
    /////////////////

    /**
     * Enumeration of Add Detector text boxes
     *
     * @author rsmith
     */
    public enum AddDetectorTextBox {
        /**
         * Width text box
         */

        WIDTH("Width (cm):", "width"),

        /**
         * Length text box
         */
        HEIGHT("Height (cm):", "height"),

        /**
         * Distance from Stop Line text box
         */
        DISTANCE("Distance (cm):", "distance"),

        /**
         * Lane text box
         */
        LANE("Lane", "lane");

        /**
         * The label of the text box as appears in the application
         */
        private String label;

        /**
         * The name of the text box as appears in the application
         */
        private String name;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        AddDetectorTextBox(String label, String name) {
            this.label = label;
            this.name = name;
        }

        /**
         * Gets the label associated with the text box as it is displayed in the
         * Web UI
         *
         * @return The label of the text box
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the name attribute associated with the text box as it is
         * displayed in the Web UI
         *
         * @return the name attribute of the text box
         */
        public String getName() {
            return this.name;
        }
    }

    /**
     * Enumeration of columns in Select Detector Lane table
     */

    public enum SelectDetectorLaneColumnHeader {

        /**
         * ID column header
         */
        ID("ID"),
        /**
         * Approach column header
         */

        APPROACH("Approach"),
        /**
         * Type column header
         */

        TYPE("Type"),
        /**
         * Movements column header
         */

        MOVEMENTS("Movements"),
        /**
         * Speed Limit column header
         */

        SPEEDLIMIT("Speed Limit (m/s)"),
        /**
         * Nodes column header
         */

        NODES("Nodes (x, y, z, w) (cm)");

        /**
         * The label of the column header as appears in the application
         */

        private String header;

        /**
         * Default constructor; sets the header
         *
         * @param header The string to set as the label
         */
        SelectDetectorLaneColumnHeader(String header) {
            this.header = header;
        }

        /**
         * Gets the header associated with the column header as it is displayed
         * in the Web UI
         *
         * @return The header label of the column header
         */
        public String getLabelHeader() {
            return this.header;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the create detector header
     */
    private static final String CREATE_DETECTOR_HEADER_TEXT = "Create Detector";

    /**
     * The xpath prefix to all text boxes in the Detector form
     */
    private static final String TEXT_BOX_XPATH_PREFIX = "//input[@name='";

    /**
     * Xpath prefix to buttons in Create Detector modal
     */
    private static final String CREATE_DETECTOR_BTN_XPATH_PREFIX = "//a[contains(@id, 'detector')][contains(@id, 'Create')]//span[contains(@id, 'button')][text()='";

    /**
     * Text displayed in the Detector help header
     */
    private static final String CREATE_DETECTOR_HELP_HEADER_TEXT = "Create Detector Help";

    /**
     * Text displayed as create detector help content
     */
    private static final String CREATE_DETECTOR_HELP_CONTENT_TEXT = "Create a new detector with the specified width (cm) and height (cm).";

    /**
     * The xpath for the Lane dropdown
     */
    private static final String LANE_DROPDOWN_XPATH = "//input[@name='lane']";

    /**
     * The xpath for the Lane ID options in the lane dropdown
     */
    private static final String LANE_OPTIONS_XPATH = "//li[@role='option'][contains(@data-boundview, 'detector')][contains(@data-boundview, 'lane')]";

    /**
     * The xpath for the lane dropdown selector
     */
    private static final String LANE_DROPDOWN_SELECTOR_XPATH = "//div[contains(@id, 'detector')][contains(@id, 'lane')][contains(@class, 'x-form-arrow-trigger')]";

    /**
     * The xpath prefix to specific Lane options
     */
    private static final String SPECIFIC_LANE_OPTION_XPATH_PREFIX = LANE_OPTIONS_XPATH + "[text()='";

    /**
     * The xpath for the show lanes button in the create detector modal
     */
    private static final String SHOW_LANES_BUTTON_XPATH = "//span[@data-ref='btnInnerEl'][text()='Show Lanes']";

    /**
     * The xpath to the lane opt private static final String
     */
    private static final String SELECTED_LANE_OPTION_XPATH = "//div[contains(@id, 'detector')][contains(@id, 'lane')]//li[contains(@class, 'x-boundlist-selected')]";

    /**
     * The constant for the attribute value used throughout the test class
     */
    private static final String ATTR_VALUE = "value";

    /**
     * The constant for the detector width error message
     */
    private static final String DETECTOR_WIDTH_NUMBER_ERROR = "Detector widths must be in the range of 1 to 400 centimeters.";

    /**
     * The constant for the detector height error message
     */
    private static final String DETECTOR_HEIGHT_NUMBER_ERROR = "Detector heights must be in the range of 1 to 800 centimeters.";

    /**
     * The constant for the detector distance number error
     */
    private static final String DETECTOR_DISTANCE_NUMBER_ERROR = "Distances from the stop line must be in the range of 0 to 25000 centimeters.";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the given button in the add detector window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(CREATE_DETECTOR_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the detector text box
     *
     * @param -the specific text box to get
     * @return the text box element
     */
    private El getTextBox(AddDetectorTextBox textBox) {
        return el(By.xpath(TEXT_BOX_XPATH_PREFIX + textBox.getName() + "']"));
    }

    /**
     * Gets the value displayed in the given text box in the Create Detector
     * modal
     *
     * @param textBox -the specific text box from which to get the value
     * @return the displayed value in the given text box
     */
    private String getTextBoxValue(AddDetectorTextBox textBox) {
        return getTextBox(textBox).getAttribute(ATTR_VALUE);
    }

    /**
     * Gets the Width value in the text box
     *
     * @return the displayed value
     */
    public String getDisplayedWidth() {
        return getTextBox(AddDetectorTextBox.WIDTH).getAttribute(ATTR_VALUE);
    }

    /**
     * Gets the height value in the text box
     *
     * @return the displayed value
     */
    public String getDisplayedHeight() {
        return getTextBox(AddDetectorTextBox.HEIGHT).getAttribute(ATTR_VALUE);
    }

    /**
     * Gets the distance value in the text box
     *
     * @return the displayed value
     */
    public String getDisplayedDistance() {
        return getTextBox(AddDetectorTextBox.DISTANCE).getAttribute(ATTR_VALUE);
    }

    /**
     * Gets specific option from dropdown list
     *
     * @param lane -the lane to get
     * @return the option element
     */
    private El getSpecificOption(String lane) {
        return el(By.xpath(SPECIFIC_LANE_OPTION_XPATH_PREFIX + lane + "']"));
    }

    /**
     * Gets the Lane dropdown menu
     *
     * @return the Lane dropdown
     */
    private El getLaneDropdown() {
        return el(By.xpath(LANE_DROPDOWN_XPATH));
    }

    /**
     * Gets the lane dropdown selector
     *
     * @return the lane dropdown selector
     */
    private El getLaneSelector() {
        return el(By.xpath(LANE_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the list of Lane options
     *
     * @return the list element
     */
    private List<El> getLaneOptions() {
        return els(By.xpath(LANE_OPTIONS_XPATH));
    }

    /**
     * Gets the value in the lane dropdown
     *
     * @return the displayed lane value
     */
    public String getLane() {
        return getLaneDropdown().getAttribute("value");
    }

    /**
     * Gets the lane value in the text box
     *
     * @return the displayed lane value
     */
    public String getDisplayedLane() {
        return getTextBox(AddDetectorTextBox.LANE).getAttribute("value");
    }

    /**
     * Get's all lanes in the select detector lane model
     *
     * @param allLanes
     * @return shows all the lanes in the select detector lane modal
     */
    private El getShowAllLanes(String allLanes) {
        return el(By.xpath(SHOW_LANES_BUTTON_XPATH));
    }

    /**
     * Get's the selected lane option from dropdown
     *
     * @return the selected lane
     */
    private El getSelectedLaneOption() {
        return el(By.xpath(SELECTED_LANE_OPTION_XPATH));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the create detector header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateDetectorHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_DETECTOR_HEADER_TEXT);
    }

    /**
     * Checks to see if the create button is displayed
     *
     * @return true if the create button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CREATE));
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.RESET));

    }

    /**
     * Checks to see if the error message associated with a number out of range
     * for the width value.
     *
     * @return the error message regarding an out of range width.
     */
    public boolean isDetectorWidthNumberErrorDisplayed() {
        return isErrorToolTipDisplayed(DETECTOR_WIDTH_NUMBER_ERROR);
    }

    /**
     * Checks to see if the error message associated with a number out of range
     * for the height value.
     *
     * @return the error message regarding an out of range height.
     */
    public boolean isDetectorHeightNumberErrorDisplayed() {
        return isErrorToolTipDisplayed(DETECTOR_HEIGHT_NUMBER_ERROR);
    }

    /**
     * Checks to see if the error message associated with a number out of range
     * for the distance value.
     *
     * @return the error message regarding an out of range distance
     */
    public boolean isDetectorDistanceNumberErrorDisplayed() {
        return isErrorToolTipDisplayed(DETECTOR_DISTANCE_NUMBER_ERROR);
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CANCEL));

    }

    /**
     * Checks to see if the Show Lanes button is displayed
     *
     * @return true if the Show Lanes button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isShowLanesBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.SHOWLANES));
    }

    /**
     * Checks to see if the Close button is displayed
     *
     * @return true if the close button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCloseBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CLOSE));
    }

    /**
     * Checks to see if the detector text box is displayed
     *
     * @param textBox -the text box expected
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isTextBoxDisplayed(AddDetectorTextBox textBox) {
        return isElementDisplayed(getTextBox(textBox));
    }

    /**
     * Checks to see if Invalid Width error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidWidthErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(AddDetectorTextBox.WIDTH.getLabel());
    }

    /**
     * Checks to see if Invalid Height error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidHeightErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(AddDetectorTextBox.HEIGHT.getLabel());
    }

    /**
     * Checks to see if an invalid distance error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidDistanceErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(AddDetectorTextBox.DISTANCE.getLabel());
    }

    /**
     * Checks to see if the create detectors help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateDetectorHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if Invalid Number error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidNonNumericHeightErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(AddDetectorTextBox.HEIGHT.getLabel());
    }

    /**
     * Checks to see if Invalid Number error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidNonNumericWidthErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(AddDetectorTextBox.WIDTH.getLabel());
    }

    /**
     * Checks to see if Invalid Number error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidNonNumericDistanceErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(AddDetectorTextBox.DISTANCE.getLabel());
    }

    /**
     * Checks to see if the create detectors help content is displayed
     *
     * @return true if the content text is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateDetectorHelpContentDisplayed() {
        return isContentDisplayed(CREATE_DETECTOR_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in the help window
     *
     * @return true if the ok button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the lane dropdown is displayed
     *
     * @return true if the lane dropdown is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isLaneDropdownDisplayed() {
        return isElementDisplayed(getLaneDropdown());
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets width value from given detector in width text box
     *
     * @param detector - the detector
     */
    public void setWidth(Detector detector) {
        String width = detector.getWidth();
        setWidth(width);
    }

    /**
     * Sets given text in width value text box
     *
     * @param width - the width value
     */
    public void setWidth(String width) {
        getTextBox(AddDetectorTextBox.WIDTH).setText(width);
    }

    /**
     * Sets height value from given detector in height text box
     *
     * @param detector - the detector
     */
    public void setHeight(Detector detector) {
        String height = detector.getHeight();
        setHeight(height);
    }

    /**
     * Sets given text in height value text box
     *
     * @param height - the height value
     */
    public void setHeight(String height) {
        getTextBox(AddDetectorTextBox.HEIGHT).setText(height);
    }

    /**
     * Sets distance value from given detector in distance box
     *
     * @param detector -the detector
     */
    public void setDistance(Detector detector) {
        String distance = detector.getDistance();
        setDistance(distance);
    }

    /**
     * Sets given text in Distance text box
     *
     * @param distance - the distance value
     */
    public void setDistance(String distance) {
        getTextBox(AddDetectorTextBox.DISTANCE).setText(distance);
    }

    /**
     * Click the Create button
     *
     * @param success -true if success is expected, false if error expected
     * @return ConbfigureDetectorsPartialPage will be returned if success is
     *         true. No return will occur if success is false.
     */
    public void clickCreate(boolean success) {
        getBtn(BtnNames.CREATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + CREATE_DETECTOR_HEADER_TEXT + "')]"));
            getPage(ConfigureDetectorsPartialPage.class);
        }
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getBtn(BtnNames.RESET).click();
    }

    /**
     * Click the Cancel button
     *
     * @return the Simulation Settings Modal
     */
    public SimulationSettingsModal clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(SimulationSettingsModal.class);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Clicks the Create Detector help icon
     */
    public void clickHelpIcon() {
        getHelpIcon(CREATE_DETECTOR_HEADER_TEXT).click();
    }

    /**
     * Clicks the close icon
     */
    public void clickCloseIcon() {
        getCloseIcon(CREATE_DETECTOR_HEADER_TEXT).click();
    }

    /**
     * Selects the Lane of the given Lane ID
     *
     * @param lane -string value of the lane to select
     */
    public void selectLane(String lane) {
        selectFromDropdownList(getLaneSelector(), getSpecificOption(lane));
    }

    /**
     * Selects the lane of the given detector
     *
     * @param detector -the detector
     */
    public void selectLane(Detector detector) {
        String lane = detector.getLane().getLaneID().getLabel();
        selectLane(lane);
    }

    /**
     * Selects the Show All Lanes Button
     *
     * @return the Select Detector Lane Modal
     */
    public SelectDetectorLaneModal clickShowAllLanesBtn() {
        getShowAllLanes(SHOW_LANES_BUTTON_XPATH).click();
        return getPage(SelectDetectorLaneModal.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkAddDetectorHeaderIcons() {
        checkHeaderIcons(CREATE_DETECTOR_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Create Detector Help header not displayed as expected.", isCreateDetectorHelpHeaderDisplayed());
        Assert.assertTrue("Create Detector Help content not displayed as expected.", isCreateDetectorHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button was not found.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button was not found.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button was not found.", isCancelBtnDisplayed());
    }

    /**
     * Checks for presence of Detector Lane dropdown,Width, Height, and Distance
     * text boxes
     */
    public void checkFieldsDisplayed() {
        Assert.assertTrue("Lane dropdown was not found.", isLaneDropdownDisplayed());
        Assert.assertTrue("Width text box was not found.", isTextBoxDisplayed(AddDetectorTextBox.WIDTH));
        Assert.assertTrue("Height text box was not found.", isTextBoxDisplayed(AddDetectorTextBox.HEIGHT));
        Assert.assertTrue("Distance text box was not found.", isTextBoxDisplayed(AddDetectorTextBox.DISTANCE));
    }

    /**
     * Sets values from given detector into Detector Lane, Width, Height, and
     * Distance text boxes
     *
     * @param detector -the detector to set
     */
    public void setAllFields(Detector detector) {
        selectLane(detector);
        setWidth(detector);
        setHeight(detector);
        setDistance(detector);
    }

    /**
     * Sets all given values from given detector into Detector Lane, Width,
     * Height, and Distance text boxes
     *
     * @param Lane -the detector lane value
     * @param width -the width value
     * @param height -the height value
     * @param distance -the distance value
     */
    public void setAllFields(String lane, String width, String height, String distance) {
        setWidth(width);
        setHeight(height);
        setDistance(distance);
    }

    /**
     * Checks values displayed in all fields and verifies they match the values
     * for the given detector
     *
     * @param detector -the detector expected
     */
    public void checkFieldValues(Detector detector) {
        String lane = detector.getLane().getLaneID().getLabel();
        String width = detector.getWidth();
        String height = detector.getHeight();
        String distance = detector.getDistance();
        checkFieldValues(lane, width, height, distance);
    }

    /**
     * Checks values displayed in all fields and verifies they match the given
     * string values
     *
     * @param lane -the lane expected
     * @param width -the width value expected
     * @param height -the height value expected
     * @param distance -the distance value expected
     */
    public void checkFieldValues(String lane, String width, String height, String distance) {
        Assert.assertEquals("The Lane ID," + lane + "is not displayed as expected.", lane, getTextBoxValue(AddDetectorTextBox.LANE));
        Assert.assertEquals("The width value," + width + ", is not displayed as expected.", width, getTextBoxValue(AddDetectorTextBox.WIDTH));
        Assert.assertEquals("The height value," + height + ", is not displayed as expected.", height, getTextBoxValue(AddDetectorTextBox.HEIGHT));
        Assert.assertEquals("The distance value," + distance + ", is not displayed as expected.", distance, getTextBoxValue(AddDetectorTextBox.DISTANCE));
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Detector
     * lane, width, height, and distance text boxes)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Lane field.", isFieldRequiredErrorDisplayed(AddDetectorTextBox.LANE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Width field.", isFieldRequiredErrorDisplayed(AddDetectorTextBox.WIDTH.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Height field.", isFieldRequiredErrorDisplayed(AddDetectorTextBox.HEIGHT.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Distance field.", isFieldRequiredErrorDisplayed(AddDetectorTextBox.DISTANCE.getLabel()));
    }

    /**
     * Click the lane dropdown and verifies the lane options list is displayed,
     * then clicks the selector again to close the list
     */
    public void checkLaneOptions() {
        clickLaneSelector();
        Assert.assertNotNull("The Lane list could not be found after clicking the Lane selector.", getLaneOptions());
        clickLaneSelector();
        waitForElementToBeInvisible(By.xpath(LANE_OPTIONS_XPATH));
    }

    /**
     * Clicks the expected lane
     */
    public void clickLaneSelector() {
        getLaneSelector().click();
    }

    /**
     * Checks the select Lane and verifies the expected lane was displayed.
     *
     * @param lane - the lane
     */
    public void checkSelectedLane(Lane lane) {
        clickLaneSelector();
        String selectedOption = getSelectedLaneOption().getText();
        Assert.assertEquals("Expected lane was not selected", lane.getLaneID().getLabel(), selectedOption);
        clickLaneSelector();
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(LANE_DROPDOWN_XPATH), 20);
    }
}
