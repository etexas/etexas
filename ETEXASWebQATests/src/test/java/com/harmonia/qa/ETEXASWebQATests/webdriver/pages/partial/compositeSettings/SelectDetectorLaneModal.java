package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneMovement;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.parital.environment.CreateDetectorModal.SelectDetectorLaneColumnHeader;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the add Detector device form
 *
 * @author rsmith
 */

public class SelectDetectorLaneModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public SelectDetectorLaneModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Xpath prefix to the Approach Cell in Select Detector Lane Modal
     */
    private static final String APPROACH_CELL_XPATH_PREFIX = ".//td[contains(@data-columnid, 'approach')]/div[text()='";

    /**
     * Xpath prefix to the Type Cell in Select Detector Lane Modal
     */
    private static final String TYPE_CELL_XPATH_PREFIX = ".//td[contains(@data-columnid, 'type')][contains(@data-columnid, 'lane')]/div[text()='";

    /**
     * Xpath prefix to the Speed Limit Cell in Select Detector Lane Modal
     */
    private static final String SPEED_LIMIT_CELL_XPATH_PREFIX = ".//td[contains(@data-columnid, 'speed')]/div[text()='";

    /**
     * Xpath to the Nodes Cell in Select Detector Lane Modal
     */
    private static final String NODES_CELL_XPATH = ".//td[contains(@data-columnid, 'nodes')]/div";

    /**
     * Xpath prefix to the Lane Movement Cell in Select Detector Lane Modal
     */
    private static final String LANE_MOVEMENT_CELL_XPATH_PREFIX = ".//td[contains(@data-columnid, 'movements')]//li[text()='";

    /**
     * Xpath prefix to the ID Cell in Select Detector Lane Modal
     */
    private static final String ID_CELL_XPATH_PREFIX = ".//td[contains(@data-columnid, 'id')]/div[text()='";

    /**
     * Xpath prefix to the Select Detector Lane Modal
     */
    private static final String SELECT_DETECTOR_LANE_MODAL_XPATH_PREFIX = "//td[contains(@class, 'lane')][contains(@class, 'id')][not(contains(@class, 'approach'))]//div[text()='";

    /**
     * Xpath Suffix to the rows in the Select Detector Lane Modal
     */
    private static final String SELECT_ROWS_DETECTOR_LANE_MODAL_XPATH_SUFFIX = "']/ancestor::tr";

    /**
     * the xpath for the Select Detector Lane header text
     */
    private static final String SELECT_DETECTOR_LANE_HEADER_TEXT = "Select Detector Lane";

    /**
     * The prefix xpath for the select detector lane table column.
     */
    private static final String SELECT_DETECTOR_LANE_TABLE_COLUMN_XPATH_PREFIX = "//div[contains(@id, 'lanegridpanel')]//span[@class='x-column-header-text-inner'][text()='";

    /**
     * Xpath prefix to buttons in Select Detector modal
     */
    private static final String SELECT_DETECTOR_LANE_BTN_XPATH_PREFIX = "//a[contains(@id, 'window-Basic')]//span[contains(@id, 'button')][text()='";

    /**
     * Xpath to the Close icon in the Select Detector Lane modal
     */
    private static final String SELECT_DETECTOR_LANE_CLOSE_ICON_XPATH = "//div[contains(@class, 'x-title-text')][text()='Select Detector Lane']/ancestor::div[contains(@class, 'x-window-header')]//div[contains(@class, 'x-tool-close')]";

    /**
     * Xpath to the Close button in the Select Detector Lane Modal
     */
    private static final String SELECT_DETECTOR_LANE_CLOSE_BTN_XPATH = "//span[contains(@id, 'Basic')][@data-ref='btnInnerEl'][text()='Close']/ancestor::a";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the Approach Cell
     *
     * @param lane - the lane
     * @return the value in the Approach Cell
     */
    private El getApproachCell(Lane lane) {
        El row = getLaneRow(lane);
        return row.el(By.xpath(APPROACH_CELL_XPATH_PREFIX + lane.getLaneApproach() + "']"));

    }

    /**
     * Gets the Type Cell
     *
     * @param lane - the lane
     * @return the value in the Type Cell
     */
    private El getTypeCell(Lane lane) {
        El row = getLaneRow(lane);
        return row.el(By.xpath(TYPE_CELL_XPATH_PREFIX + lane.getLaneType() + "']"));
    }

    /**
     * Gets the Speed Limit cell
     *
     * @param lane - the lane
     * @return the value in the Speed Limit Cell
     */
    private El getSpeedLimitCell(Lane lane) {
        El row = getLaneRow(lane);
        return row.el(By.xpath(SPEED_LIMIT_CELL_XPATH_PREFIX + lane.getSpeedLimit() + "']"));
    }

    /**
     * Gets the Node cell
     *
     * @param lane- the lane
     * @return the node cell
     */
    private El getNodesCell(Lane lane) {
        El row = getLaneRow(lane);
        return row.el(By.xpath(NODES_CELL_XPATH));
    }

    /**
     * Gets the Lane Movement cell
     *
     * @param lane -the lane
     * @param laneMovement -the lane movement expected
     * @return the lane movement cell element
     */
    private El getLaneMovementCell(Lane lane, LaneMovement laneMovement) {
        El row = getLaneRow(lane);
        return row.el(By.xpath(LANE_MOVEMENT_CELL_XPATH_PREFIX + laneMovement + "']"));
    }

    /**
     * Gets the start node value displayed for the given lane
     *
     * @param lane -the lane
     * @return the start node value displayed
     */
    private String getStartNodeValue(Lane lane) {
        El nodeCell = getNodesCell(lane);
        String nodeValue = nodeCell.getText();
        String[] startValueArray = nodeValue.split("\n");
        return startValueArray[0];

    }

    /**
     * Gets the end node value displayed for the given lane
     *
     * @param lane -the lane
     * @return the start node value displayed
     */
    private String getEndNodeValue(Lane lane) {
        El nodeCell = getNodesCell(lane);
        String nodeValue = nodeCell.getText();
        String[] endValueArray = nodeValue.split("\n");
        return endValueArray[1];
    }

    /**
     * Gets the Lane ID Cell
     *
     * @param lane - the lane selected
     * @return the Lane ID value
     */
    private El getIdCell(Lane lane) {
        El row = getLaneRow(lane);
        return row.el(By.xpath(ID_CELL_XPATH_PREFIX + lane.getLaneID() + "']"));
    }

    /**
     * Gets the Lane Row
     *
     * @param lane - the lane to select
     * @return the Lane Modal with the lane ID in the row
     */
    private El getLaneRow(Lane lane) {
        String id = lane.getLaneID().getLabel();
        return el(By.xpath(SELECT_DETECTOR_LANE_MODAL_XPATH_PREFIX + id + SELECT_ROWS_DETECTOR_LANE_MODAL_XPATH_SUFFIX));
    }

    /**
     * Gets the given button in the select detector lane modal
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(SELECT_DETECTOR_LANE_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the given column header element
     *
     * @param column -the column to get
     * @return the column header element
     */

    private El getSelectLaneDetectorColumnHeader(SelectDetectorLaneColumnHeader header) {
        return el(By.xpath(SELECT_DETECTOR_LANE_TABLE_COLUMN_XPATH_PREFIX + header.getLabelHeader() + "']"));
    }

    /**
     * Gets the Select Detector Lane modal close icon
     *
     * @return the close icon element
     */
    private El getSelectLaneModalCloseIcon() {
        return el(By.xpath(SELECT_DETECTOR_LANE_CLOSE_ICON_XPATH));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the Approach cell is displayed
     *
     * @param lane - the lane to select
     * @return true if the Approach cell is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isApproachCellDisplayed(Lane lane) {
        return isElementDisplayed(getApproachCell(lane));
    }

    /**
     * Checks to see if the Type Cell is displayed
     *
     * @param lane- the lane to select
     * @return true if the Type Cell is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isTypeCellDisplayed(Lane lane) {
        return isElementDisplayed(getTypeCell(lane));
    }

    /**
     * Checks to see if the speed limit cell is displayed
     *
     * @param lane- the lane to select
     * @return true if the speed limit cell is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isSpeedLimitCellDisplayed(Lane lane) {
        return isElementDisplayed(getSpeedLimitCell(lane));
    }

    /**
     * Checks to see if Node Cell is displayed
     *
     * @param lane- the lane to select
     * @return true if the Node Cell is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isNodesCelDisplayed(Lane lane) {
        return isElementDisplayed(getNodesCell(lane));
    }

    /**
     * Checks to see if the Lane ID cell is displayed
     *
     * @param lane - the lane to select
     * @return true if the Lane ID cell is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isIdCellDisplayed(Lane lane) {
        return isElementDisplayed(getIdCell(lane));
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
     * Checks if lane movement cell is displayed containing given text in given
     * lane
     *
     * @param lane -the lane to check
     * @param laneMovement -the lane movement expected
     * @return true if displayed, false if not
     */
    public boolean isLaneMovementCellDisplayed(Lane lane, LaneMovement laneMovement) {
        return isElementDisplayed(getLaneMovementCell(lane, laneMovement));
    }

    /**
     * Checks to see if the column header expected is displayed
     *
     * @param column -the column expected
     * @return true if displayed, false otherwise
     */
    public boolean isSelectDetectorLaneColumnHeaderDisplayed(SelectDetectorLaneColumnHeader header) {
        return isElementDisplayed(getSelectLaneDetectorColumnHeader(header));
    }

    /**
     * Selects the row for the given lane
     *
     * @param lane -the lane to select
     */
    public void selectDetectorLane(Lane lane) {
        getLaneRow(lane).click();

    }

    /**
     * Selects the row for the given detector
     *
     * @param detector - the detector to select
     */
    public void selectDetectorLane(Detector detector) {
        Lane lane = detector.getLane();
        waitUntilLoaded();
        selectDetectorLane(lane);
    }

    /**
     * Checks to see if the select detector header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isSelectDetectorLaneHeaderDisplayed() {
        return isHeaderDisplayed(SELECT_DETECTOR_LANE_HEADER_TEXT);
    }

    /**
     * Checks to see if the Close button is displayed in the Select Detector
     * Lane modal
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSelectDetectorLaneCloseBtnDisplayed() {
        return isBtnDisplayed(SELECT_DETECTOR_LANE_HEADER_TEXT, BtnNames.CLOSE.getLabel());
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Clicks the close icon
     */
    public void clickCloseIcon() {
        getSelectLaneModalCloseIcon().click();
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
        El row = getLaneRow(lane);
        Assert.assertTrue("Lane type, " + type + ", could not be found in the row with Lane ID of " + laneID + ".", isTypeCellDisplayed(lane));
        Assert.assertTrue("Lane approach, " + approach + ", could not be found in the row with Lane ID of " + laneID + ".", isApproachCellDisplayed(lane));
        Assert.assertTrue("Speed limit of " + speedLimit + " could not be found in the row with Lane ID of " + laneID + ".", isSpeedLimitCellDisplayed(lane));
        Assert.assertEquals("Starting node of " + startingNode + " could not be found in the row with Lane ID of " + laneID + ".", startingNode, getStartNodeValue(lane));
        Assert.assertEquals("Ending node limit of " + endingNode + " could not be found in the row with Lane ID of " + laneID + ".", endingNode, getEndNodeValue(lane));
        /**
         * Lists the lane movements associated with the given lane ID.
         */
        List<LaneMovement> movements = lane.getLaneMovements();
        if (movements != null) {
            for (LaneMovement movement : movements) {
                Assert.assertTrue("Lane movement, " + movement.getLabel() + ", could not be found in the row with Lane ID of " + laneID + ".", isLaneMovementCellDisplayed(lane, movement));
            }
        }
    }

    /**
     * Checks to see if the Column Headers are displayed in the Select Detector
     * Lane Modal
     */
    public void checkSelectDetectorLaneModalColumnHeaders() {
        Assert.assertTrue("The ID column header not displayed as expected in Select Detector Lane Modal", isSelectDetectorLaneColumnHeaderDisplayed(SelectDetectorLaneColumnHeader.ID));
        Assert.assertTrue("Approach header cell not displayed as expected.", isSelectDetectorLaneColumnHeaderDisplayed(SelectDetectorLaneColumnHeader.APPROACH));
        Assert.assertTrue("Type header cell not displayed as expected.", isSelectDetectorLaneColumnHeaderDisplayed(SelectDetectorLaneColumnHeader.TYPE));
        Assert.assertTrue("Movements header cell not displayed as expected.", isSelectDetectorLaneColumnHeaderDisplayed(SelectDetectorLaneColumnHeader.MOVEMENTS));
        Assert.assertTrue("Speed Limit header cell not displayed as expected.", isSelectDetectorLaneColumnHeaderDisplayed(SelectDetectorLaneColumnHeader.SPEEDLIMIT));
        Assert.assertTrue("Nodes header cell not displayed as expected.", isSelectDetectorLaneColumnHeaderDisplayed(SelectDetectorLaneColumnHeader.NODES));
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

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(SELECT_DETECTOR_LANE_CLOSE_BTN_XPATH), 20);
    }
}