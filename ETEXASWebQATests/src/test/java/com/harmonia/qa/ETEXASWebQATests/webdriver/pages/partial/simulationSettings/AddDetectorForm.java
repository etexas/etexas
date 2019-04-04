package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings;

import java.util.Iterator;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneMovement;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.LaneGeometryPartialPage.LaneGeometryHeaderCells;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the add detectors form
 *
 * @author llaroussini
 */
public class AddDetectorForm extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public AddDetectorForm(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    //////////////////
    // Enumerations
    /////////////////

    /**
     * Enumeration of Add Detector text boxes
     *
     * @author llaroussini
     */
    public enum AddDetectorTextBox {
        /**
         * Width text box
         */
        WIDTH("Width (cm):"),
        /**
         * Length text box
         */
        LENGTH("Length (cm):"),
        /**
         * Distance from Stop Line text box
         */
        DISTANCE_STOP_LINE("Distance from Stop Line (cm):");

        /**
         * The label of the text box as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        AddDetectorTextBox(String label) {
            this.label = label;
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
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the add detectors header
     */
    private static final String ADD_DETECTOR_HEADER_TEXT = "Add Detector";

    /**
     * Text displayed in the add detectors help header
     */
    private static final String ADD_DETECTOR_HELP_HEADER_TEXT = "Add Detector Help";

    /**
     * Text displays as add detectors help content
     */
    private static final String ADD_DETECTOR_HELP_CONTENT_TEXT = "The Add Detector dialog allows the user to select a lane and then enter detector information on the right side of the display to add a detector to the lane.  Detectors may also be deleted from lanes on this page as well.";

    /**
     * Xpath to the Add Detector window
     */
    private static final String ADD_DETECTOR_WINDOW_XPATH = "//div[contains(@id, 'addDetectorForm')]";

    /**
     * Xpath the Select Lane section
     */
    private static final String SELECT_LANE_SECTION_XPATH = "//div[contains(@id, 'laneGeometryTable')]";

    /**
     * Xpath the Detector Values section
     */
    private static final String DETECTOR_VALUES_SECTION_XPATH = "//div[contains(text(), 'Enter New Detector Values')]/ancestor::div[contains(@id, 'form')]";

    /**
     * Xpath to Section title
     */
    private static final String SECTION_TITLE_XPATH = ".//div[contains(@class, 'x-title-text')]";

    /**
     * Header text displayed in Select Lane section
     */
    private static final String SELECT_LANE_HEADER_TEXT = "1. Select Lane";

    /**
     * Header text displayed in Enter Detector Values section
     */
    private static final String DETECTOR_VALUES_HEADER_TEXT = "2. Enter New Detector Values";

    /**
     * Xpath prefix to a specific cell in table (with known value)
     */
    protected static final String SPECIFIC_CELL_XPATH_PREFIX = ".//div[@class='x-grid-cell-inner '][text()='";

    /**
     * Xpath prefix to node cell
     */
    private static final String NODE_CELL_XPATH_PREFIX = ".//span[contains(@data-qtip, '";

    /**
     * Xpath prefix to lane geometry column header cells
     */
    private static final String LANE_COLUMN_HEADER_CELL_XPATH_PREFIX = "//div[contains(@id, 'laneGeometryTable')]//span[text()='";

    /**
     * Xpath prefix used for locating a specific text box
     */
    private static final String TEXT_BOX_XPATH_PREFIX = "//span[text()='";

    /**
     * Xpath suffix used for locating a specific text box
     */
    private static final String TEXT_BOX_XPATH_SUFFIX = "']/ancestor::node()[2]/div//input";

    /**
     * Xpath to the Lane table
     */
    private static final String LANE_TABLE_XPATH = "//div[text()='1. Select Lane']//ancestor::div//div[@class='x-grid-item-container']";

    /**
     * Class of a selected lane
     */
    private static final String SELECTED_LANE_CLASS = "x-grid-item-selected";

    /**
     * Text displayed when maximum width is exceeded
     */
    private static final String MAX_WIDTH_ERROR_TEXT = "The maximum value for this field is 400.";

    /**
     * Text displayed when minimum width is used
     */
    private static final String MIN_WIDTH_ERROR_TEXT = "The minimum value for this field is 1.";

    /**
     * Text displayed when maximum length is exceeded
     */
    private static final String MAX_LENGTH_ERROR_TEXT = "The maximum value for this field is 800.";

    /**
     * Text displayed when minimum length is used
     */
    private static final String MIN_LENGTH_ERROR_TEXT = "The minimum value for this field is 1.";

    /**
     * Text displayed when maximum distance from stop line is exceeded
     */
    private static final String MAX_DISTANCE_FROM_STOP_LINE_ERROR_TEXT = "The maximum value for this field is 25000.";

    /**
     * Text displayed when minimum distance from stop line is used
     */
    private static final String MIN_DISTANCE_FROM_STOP_LINE_ERROR_TEXT = "The value cannot be negative.";

    /**
     * Xpath to Lane Selection Error window
     */
    private static final String LANE_SELECTION_ERROR_WINDOW_XPATH = "//div[contains(@id, 'messagebox')][text()='Please select a lane']";

    /**
     * Xpath to OK button in Lane Selection Error window
     */
    private static final String LANE_SELECTION_ERROR_OK_BTN_XPATH = LANE_SELECTION_ERROR_WINDOW_XPATH + "/ancestor::div//a//span[text()='OK']";

    //////////////
    // Getters
    /////////////

    /**
     * Gets the Add Detector window
     *
     * @return the window element
     */
    private El getAddDetectorWindow() {
        return el(By.xpath(ADD_DETECTOR_WINDOW_XPATH));
    }

    /**
     * The xpath to get from the add detectors header to the help icon
     *
     * @return the help icon
     */
    private El getAddDetectorsHelpIcon() {
        return getHelpIcon(ADD_DETECTOR_HEADER_TEXT);
    }

    /**
     * Gets the Select Lane section
     *
     * @return the section element
     */
    private El getSelectLaneSection() {
        return el(By.xpath(SELECT_LANE_SECTION_XPATH));
    }

    /**
     * Gets the Enter Detector Values section
     *
     * @return the section element
     */
    private El getEnterDetectorValuesSection() {
        return el(By.xpath(DETECTOR_VALUES_SECTION_XPATH));
    }

    /**
     * Gets the Select Lane section title
     *
     * @return the section title element
     */
    private El getSelectLaneSectionTitle() {
        return getSelectLaneSection().el(By.xpath(SECTION_TITLE_XPATH));
    }

    /**
     * Gets the Enter Detector Values section title
     *
     * @return the section title element
     */
    private El getEnterDetectorValuesSectionTitle() {
        return getEnterDetectorValuesSection().el(By.xpath(SECTION_TITLE_XPATH));
    }

    /**
     * Gets the given column header cell
     *
     * @param headerCell -the header cell to get
     * @return the cell element
     */
    private El getHeaderCell(LaneGeometryHeaderCells headerCell) {
        return el(By.xpath(LANE_COLUMN_HEADER_CELL_XPATH_PREFIX + headerCell.getLabel() + "']"));
    }

    /**
     * Gets cell with given info in the given row
     *
     * @param row -the row where info is expected
     * @param info -the info expected
     * @return the cell element
     */
    private El getInfoCell(El row, String info) {
        return row.el(By.xpath(SPECIFIC_CELL_XPATH_PREFIX + info + "']"));
    }

    /**
     * Gets lane row based on given lane ID
     *
     * @param laneID -the lane ID expected
     * @param type -the lane type
     * @return the row element
     */
    private El getLaneRow(String laneID, String type) {
        List<El> rows = rowsInTable(type, getLaneTable());
        El theRow = null;
        List<El> newRows = rows;

        Iterator<El> newIterator = newRows.iterator();
        while (newIterator.hasNext() && theRow == null) {
            El newRow = newIterator.next();

            El firstCell = newRow.el(By.xpath(".//td[contains(@class, 'x-grid-cell')]/div"));
            String cellText = firstCell.getText();
            if (cellText.contains(laneID)) {
                theRow = newRow;
                break;
            }
        }
        return theRow;

    }

    /**
     * Gets node cell that contains given text in the given row
     *
     * @param row -the row where info is expected
     * @param text -the text expected
     * @return the cell element
     */
    private El getLaneNodeCell(El row, String text) {
        return row.el(By.xpath(NODE_CELL_XPATH_PREFIX + text + "')]"));
    }

    /**
     * Gets movement cell that contains given text in the given row
     *
     * @param row -the row where info is expected
     * @param text -the text expected
     * @return the cell element
     */
    private El getLaneMovementCell(El row, String text) {
        List<El> cells = row.els(By.xpath(".//ul"));
        El theCell = null;
        for (El cell : cells) {
            String displayedText = cell.getAttribute("data-qtip");
            if (displayedText.contains(text)) {
                theCell = cell;
            }
        }
        return theCell;
    }

    /**
     * Gets the specified text box
     *
     * @param textBox - the text box to get
     * @return the text box
     */
    private El getTextBox(AddDetectorTextBox textBox) {
        return el(By.xpath(TEXT_BOX_XPATH_PREFIX + textBox.getLabel() + TEXT_BOX_XPATH_SUFFIX));
    }

    /**
     * Gets the create button
     *
     * @return the create button
     */
    private El getCreateBtn() {
        return getAddDetectorWindow().el(By.xpath(FORM_BTN_XPATH_PREFIX + BtnNames.CREATE.getLabel() + "']"));
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getAddDetectorWindow().el(By.xpath(FORM_BTN_XPATH_PREFIX + BtnNames.RESET.getLabel() + "']"));
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getAddDetectorWindow().el(By.xpath(FORM_BTN_XPATH_PREFIX + BtnNames.CANCEL.getLabel() + "']"));
    }

    /**
     * Gets the Lane table xpath
     *
     * @return the lane table xpath
     */
    private El getLaneTable() {
        return el(By.xpath(LANE_TABLE_XPATH));
    }

    /**
     * Gets row with the given lane
     *
     * @param laneID -the id of the lane to find
     * @return the row element
     */
    private El getLaneRow(String laneID) {
        return rowInTable(laneID, getLaneTable(), ".//table");
    }

    /**
     * Gets the Lane Selection Error pop-up window
     *
     * @return the pop-up window element
     */
    private El getLaneSelectionErrorWindow() {
        return el(By.xpath(LANE_SELECTION_ERROR_WINDOW_XPATH));
    }

    /**
     * Gets the OK button in the Lane Selection Error pop-up window
     *
     * @return the button element
     */
    private El getLaneSelectionWindowOKBtn() {
        return el(By.xpath(LANE_SELECTION_ERROR_OK_BTN_XPATH));
    }

    /**
     * Gets the value in the width text box
     *
     * @return the displayed Width value
     */
    public String getDisplayedWidth() {
        return getTextBox(AddDetectorTextBox.WIDTH).getAttribute("value");
    }

    /**
     * Gets the value in the length text box
     *
     * @return the displayed Length value
     */
    public String getDisplayedLength() {
        return getTextBox(AddDetectorTextBox.LENGTH).getAttribute("value");
    }

    /**
     * Gets the value in the distance from stop line text box
     *
     * @return the displayed distance from stop line value
     */
    public String getDisplayedDistanceFromStopLine() {
        return getTextBox(AddDetectorTextBox.DISTANCE_STOP_LINE).getAttribute("value");
    }

    //////////////
    // Checkers
    /////////////

    /**
     * Checks to see if the add detector window is displayed
     *
     * @return true if the window is displayed, false otherwise
     */
    public boolean isAddDetectorWindowDisplayed() {
        return isElementDisplayed(getAddDetectorWindow());
    }

    /**
     * Checks to see if the add detector header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isAddDetectorHeaderDisplayed() {
        return isHeaderDisplayed(ADD_DETECTOR_HEADER_TEXT);
    }

    /**
     * Checks to see if the add detector help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isAddDetectorHelpHeaderDisplayed() {
        return isHeaderDisplayed(ADD_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the add detectors help content is displayed
     *
     * @return true if the content text is displayed, false otherwise
     */
    public boolean isAddDetectorHelpContentDisplayed() {
        return isContentDisplayed(ADD_DETECTOR_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in the help window
     *
     * @return true if the ok button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(ADD_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Checks if Select Lane section is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSelectLaneSectionDisplayed() {
        return isElementDisplayed(getSelectLaneSection());
    }

    /**
     * Checks if Enter Detector Values section is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isEnterDetectorValuesSectionDisplayed() {
        return isElementDisplayed(getEnterDetectorValuesSection());
    }

    /**
     * Checks if given header cell is displayed
     *
     * @param headerCell -the column header cell expected
     * @return true if displayed, false if not found or not visible
     */
    public boolean isHeaderCellDisplayed(LaneGeometryHeaderCells headerCell) {
        return isElementDisplayed(getHeaderCell(headerCell));
    }

    /**
     * Checks if info cell is displayed with given info in given lane row
     *
     * @param laneRow -the row where info is expected
     * @param info -the info expected
     * @return true if displayed, false if not
     */
    private boolean isLaneInfoCellDisplayed(El laneRow, String info) {
        return isElementDisplayed(getInfoCell(laneRow, info));
    }

    /**
     * Checks if lane node cell is displayed containing given text in given lane
     * row
     *
     * @param laneRow -the row where info is expected
     * @param text -the text expected
     * @return true if displayed, false if not
     */
    private boolean isLaneNodeCellDisplayed(El laneRow, String text) {
        return isElementDisplayed(getLaneNodeCell(laneRow, text));
    }

    /**
     * Checks if lane movement cell is displayed containing given text in given
     * lane row
     *
     * @param laneRow -the row where info is expected
     * @param text -the text expected
     * @return true if displayed, false if not
     */
    private boolean isLaneMovementCellDisplayed(El laneRow, String text) {
        try {
            El cell = getLaneMovementCell(laneRow, text);
            return cell != null; //Note: despite being present on the page the isDisplayed() call returns false.
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the create button is displayed
     *
     * @return true if the create button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getCreateBtn());
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the given text box is displayed
     *
     * @param textBox - the text box expected
     * @return true if the width text box is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isTextBoxDisplayed(AddDetectorTextBox textBox) {
        return isElementDisplayed(getTextBox(textBox));
    }

    /**
     * Checks if the given lane is selected
     *
     * @param lane -the lane to check
     * @return true if selected, false otherwise
     */
    public boolean isLaneSelected(Lane lane) {
        El laneRow = getLaneRow(lane.getLaneID().getLabel());
        String laneClass = laneRow.getAttribute("class");
        if (laneClass.contains(SELECTED_LANE_CLASS)) {
            return true;
        }
        else {
            return false;
        }
    }

    /**
     * Checks if Maximum Width Error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaxWidthErrorDisplayed() {
        return getSpecificErrorToolTip(MAX_WIDTH_ERROR_TEXT) != null;
    }

    /**
     * Checks if Minimum Width Error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMinWidthErrorDisplayed() {
        return getSpecificErrorToolTip(MIN_WIDTH_ERROR_TEXT) != null;
    }

    /**
     * Checks if Maximum Length Error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaxLengthErrorDisplayed() {
        return getSpecificErrorToolTip(MAX_LENGTH_ERROR_TEXT) != null;
    }

    /**
     * Checks if Minimum Length Error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMinLengthErrorDisplayed() {
        return getSpecificErrorToolTip(MIN_LENGTH_ERROR_TEXT) != null;
    }

    /**
     * Checks if Maximum Distance from Stop Line Error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMaxDistanceFromStopLineErrorDisplayed() {
        return getSpecificErrorToolTip(MAX_DISTANCE_FROM_STOP_LINE_ERROR_TEXT) != null;
    }

    /**
     * Checks if Minimum Distance from Stop Line Error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isMinDistanceFromStopLineErrorDisplayed() {
        return getSpecificErrorToolTip(MIN_DISTANCE_FROM_STOP_LINE_ERROR_TEXT) != null;
    }

    /**
     * Checks if Lane Selection Error Window is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLaneSelectionErrorWindowDisplayed() {
        return isElementDisplayed(getLaneSelectionErrorWindow());
    }

    /**
     * Checks if OK button in Lane Selection Error Window is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLaneSelectionErrorOKBtnDisplayed() {
        return isElementDisplayed(getLaneSelectionWindowOKBtn());
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Click the Add Detectors Help icon
     */
    public void clickAddDetectorsHelpIcon() {
        getAddDetectorsHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(ADD_DETECTOR_HELP_HEADER_TEXT);
    }

    /**
     * Click the Create button when error expected
     */
    public void clickCreateErrorExepected() {
        getCreateBtn().click();
    }

    /**
     * Click the Create button
     *
     * @return the newly loaded Configure Detectors Form
     */
    public SimulationSettingsModal clickCreate() {
        getCreateBtn().click();
        waitForElementToBeInvisible(By.xpath(ADD_DETECTOR_WINDOW_XPATH));
        return getPage(SimulationSettingsModal.class);
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
     * @return the newly loaded Configure Devices Form
     */
    public SimulationSettingsModal clickCancel() {
        getCancelBtn().click();
        return getPage(SimulationSettingsModal.class);
    }

    /**
     * Selects the row for the given lane
     *
     * @param lane -the lane to select
     */
    public void selectLane(Lane lane) {
        String laneID = lane.getLaneID().getLabel();
        selectLane(laneID);
    }

    /**
     * Selects the row for the given lane id
     *
     * @param laneID -the id of the lane to select
     */
    public void selectLane(String laneID) {
        getLaneRow(laneID).click();
    }

    /**
     * Sets given width in width text box
     *
     * @param width -the width to enter
     */
    public void setWidthValue(String width) {
        getTextBox(AddDetectorTextBox.WIDTH).setText(width);
    }

    /**
     * Sets the width of the given detector in width text box
     *
     * @param detector -the detector who's width should be entered
     */
    public void setWidthValue(Detector detector) {
        String width = detector.getWidth();
        setWidthValue(width);
    }

    /**
     * Sets given length in length text box
     *
     * @param length -the length to enter
     */
    public void setLengthValue(String length) {
        getTextBox(AddDetectorTextBox.LENGTH).setText(length);
    }

    /**
     * Sets the length of the given detector in length text box
     *
     * @param detector -the detector who's length should be entered
     */
    public void setLengthValue(Detector detector) {
        String length = detector.getHeight();
        setLengthValue(length);
    }

    /**
     * Sets given distance in distance from stop line text box
     *
     * @param distance -the distance to enter
     */
    public void setDistanceValue(String distance) {
        getTextBox(AddDetectorTextBox.DISTANCE_STOP_LINE).setText(distance);
    }

    /**
     * Sets the distance of the given detector in distance from stop line text
     * box
     *
     * @param detector -the detector who's distance should be entered
     */
    public void setDistanceValue(Detector detector) {
        String distance = detector.getDistance();
        setDistanceValue(distance);
    }

    /**
     * Sets the lane/width/length/distance from stop line values from the given
     * detector
     *
     * @param detector -the detector who's values should be set
     */
    public void setDetectorValues(Detector detector) {
        selectLane(detector.getLane());
        setWidthValue(detector);
        setLengthValue(detector);
        setDistanceValue(detector);
    }

    /**
     * Clicks the OK button in the Lane Selection Error window
     */
    public void clickLaneSelectionErrorOKBtn() {
        getLaneSelectionWindowOKBtn().click();
    }

    //////////////
    // Utilities
    /////////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkAddDetectorsHeaderIcons() {
        checkHeaderIcons(ADD_DETECTOR_HEADER_TEXT);
    }

    /**
     * Checks for presence of Select Lane and Enter Detector Values sections
     */
    public void checkSectionsDisplayed() {
        Assert.assertTrue("Select Lane section not displayed as expected.", isSelectLaneSectionDisplayed());
        Assert.assertTrue("Enter Detector Values section not displayed as expected.", isEnterDetectorValuesSectionDisplayed());
    }

    /**
     * Checks that section header (Select Lane and Enter Detector Values)
     * display expected header text
     */
    public void checkSectionHeaders() {
        String displayedSelectLaneText = getSelectLaneSectionTitle().getText();
        String displayedDetectorValuesText = getEnterDetectorValuesSectionTitle().getText();
        Assert.assertEquals("Select Lane header text no displayed as expected.", SELECT_LANE_HEADER_TEXT, displayedSelectLaneText);
        Assert.assertEquals("Enter Detector Values header text no displayed as expected.", DETECTOR_VALUES_HEADER_TEXT, displayedDetectorValuesText);
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button not displayed as expected.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button not displayed as expected.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button not displayed as expected.", isCancelBtnDisplayed());
    }

    /**
     * Checks for the presence of the Width, Length, and Distance from Stop Line
     * text boxes
     */
    public void checkFields() {
        Assert.assertTrue("The Width text box not displayed as expected.", isTextBoxDisplayed(AddDetectorTextBox.WIDTH));
        Assert.assertTrue("The Length text box not displayed as expected.", isTextBoxDisplayed(AddDetectorTextBox.LENGTH));
        Assert.assertTrue("The Distance From Stop Line text box not displayed as expected.", isTextBoxDisplayed(AddDetectorTextBox.DISTANCE_STOP_LINE));
    }

    /**
     * Checks for presence of all column header cells (Lane ID, Approach, Type,
     * Movements Speed Limit, Nodes)
     */
    public void checkAllColumnHeaderCells() {
        Assert.assertTrue("Lane ID column header cell could not be found or is not visible.", isHeaderCellDisplayed(LaneGeometryHeaderCells.LANE_ID));
        Assert.assertTrue("Approach column header cell could not be found or is not visible.", isHeaderCellDisplayed(LaneGeometryHeaderCells.APPROACH));
        Assert.assertTrue("Type column header cell could not be found or is not visible.", isHeaderCellDisplayed(LaneGeometryHeaderCells.TYPE));
        Assert.assertTrue("Movements column header cell could not be found or is not visible.", isHeaderCellDisplayed(LaneGeometryHeaderCells.MOVEMENTS));
        Assert.assertTrue("Speed Limit column header cell could not be found or is not visible.", isHeaderCellDisplayed(LaneGeometryHeaderCells.SPEED_LIMIT));
        Assert.assertTrue("Nodes column header cell could not be found or is not visible.", isHeaderCellDisplayed(LaneGeometryHeaderCells.NODES));
    }

    /**
     * Checks known values for a lane row and asserts they can be found as
     * displayed in the row (row is found by given laneID)
     *
     * @param lane -the lane to check
     */
    public void checkLaneInfo(Lane lane) {
        String laneID = lane.getLaneID().getLabel();
        String approach = lane.getLaneApproach();
        String type = lane.getLaneType().getLabel();
        String speedLimit = lane.getSpeedLimit();
        String startingNode = lane.getLaneStartNode();
        String endingNode = lane.getLaneEndNode();
        El row = getLaneRow(laneID, type);
        Assert.assertTrue("Lane type, " + type + ", could not be found in the row with Lane ID of " + laneID + ".", isLaneInfoCellDisplayed(row, type));
        Assert.assertTrue("Lane approach, " + approach + ", could not be found in the row with Lane ID of " + laneID + ".", isLaneInfoCellDisplayed(row, approach));
        Assert.assertTrue("Speed limit of " + speedLimit + " could not be found in the row with Lane ID of " + laneID + ".", isLaneInfoCellDisplayed(row, speedLimit));
        Assert.assertTrue("Starting node of " + startingNode + " could not be found in the row with Lane ID of " + laneID + ".", isLaneNodeCellDisplayed(row, startingNode));
        Assert.assertTrue("Ending node limit of " + endingNode + " could not be found in the row with Lane ID of " + laneID + ".", isLaneNodeCellDisplayed(row, endingNode));
        List<LaneMovement> movements = lane.getLaneMovements();
        if (movements != null) {
            for (LaneMovement movement : movements) {
                String laneMovement = movement.getLabel();
                Assert.assertTrue("Lane movement, " + laneMovement + ", could not be found in the row with Lane ID of " + laneID + ".", isLaneMovementCellDisplayed(row, laneMovement));
            }
        }
    }

    /**
     * Checks info (ID, approach, type, movements, speed limit, starting node,
     * and ending node) for all lanes associated with the given simulation
     *
     * @param sim -the simulation with lanes to check
     */
    public void checkAllLanes(TemplateSimulation sim) {
        List<Lane> lanes = sim.getLanes();
        for (Lane lane : lanes) {
            checkLaneInfo(lane);
        }
    }

    /**
     * Checks that displayed values match given values
     *
     * @param width -the width expected to be selected
     * @param length -the length expected to be displayed
     * @param dist -the distance from stop line expected to be displayed
     */
    public void checkDisplayedSetFields(String width, String length, String dist) {
        Assert.assertEquals("Displayed width does not match expected width.", width, getDisplayedWidth());
        Assert.assertEquals("Displayed length does not match expected length.", length, getDisplayedLength());
        Assert.assertEquals("Displayed distance from stop line does not match expected distance from stop line.", dist, getDisplayedDistanceFromStopLine());
    }

    /**
     * Checks that displayed values match values associated with given detector
     * for width, length, and distance from stop line
     *
     * @param detector- the detector expected
     */
    public void checkDisplayedSetFields(Detector detector) {
        String width = detector.getWidth();
        String length = detector.getHeight();
        String dist = detector.getDistance();
        checkDisplayedSetFields(width, length, dist);
    }

    /**
     * Checks if given lane selection state based on boolean param
     *
     * @param lane -the lane to check
     * @param selected -true if lane is expected to be selected, false otherwise
     */
    public void checkLaneSelection(Lane lane, boolean selected) {
        String laneID = lane.getLaneID().getLabel();
        if (selected == true) {
            Assert.assertTrue("Lane with ID of " + laneID + " is not selected as expected.", isLaneSelected(lane));
        }
        else {
            Assert.assertFalse("Lane with ID of " + laneID + " is selected.", isLaneSelected(lane));
        }
    }

    /**
     * Checks if lane associated with given detector selection state based on
     * boolean param
     *
     * @param detector -the detector expected
     * @param selected -true if lane associated with detector is expected to be
     *        selected, false otherwise
     */
    public void checkLaneSelection(Detector detector, boolean selected) {
        Lane lane = detector.getLane();
        checkLaneSelection(lane, selected);
    }

    /**
     * Checks that Lane selection, Width, Length, and Distance values are all
     * cleared
     *
     * @param lane -the lane that was previously selected
     */
    public void checkFieldsCleared(Lane lane) {
        //checkLaneSelection(lane, false); //BUG 12320
        checkDisplayedSetFields("", "", "");
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Width,
     * Length, and Distance from Stop Line text boxes)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Width field.", isFieldRequiredErrorDisplayed(AddDetectorTextBox.LENGTH.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Length field.", isFieldRequiredErrorDisplayed(AddDetectorTextBox.WIDTH.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Distance from Stop Line field.", isFieldRequiredErrorDisplayed(AddDetectorTextBox.DISTANCE_STOP_LINE.getLabel()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(SELECT_LANE_SECTION_XPATH));
    }
}
