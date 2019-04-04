package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.Iterator;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneMovement;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * The partial page displayed as a part of the Execution Details page when the
 * Lane Geometry tab is selected
 *
 * @author llaroussini
 */
public class LaneGeometryPartialPage extends ExecutionPartialPage {

    /**
     * Default constructor
     * 
     * @param driver the web driver instance being used
     */
    public LaneGeometryPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Lane Geometry cell titles
     *
     * @author llaroussini
     */
    public enum LaneGeometryHeaderCells {
        /**
         * The lane ID title for header cell
         */
        LANE_ID("Lane ID"),
        /**
         * The Approach title for header cell
         */
        APPROACH("Approach"),
        /**
         * The Type title for header cell
         */
        TYPE("Type"),
        /**
         * The Movements title for header cell
         */
        MOVEMENTS("Movements"),
        /**
         * The Speed Limit title for header cell
         */
        SPEED_LIMIT("Speed Limit (m/s)"),
        /**
         * The Nodes title for header cell
         */
        NODES("Nodes (x, y, z, w) (cm)");

        /**
         * The label of the titles of the header cells as they appear in the
         * application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        LaneGeometryHeaderCells(String label) {
            this.label = label;
        }

        /**
         * Gets the label (title) associated with the header cell as it is
         * displayed in the Web UI
         *
         * @return The label of the header cell
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath to the Lane table
     */
    private static final String LANE_TABLE_XPATH = "//div[contains(@id, 'laneGeometryTable')]";

    /**
     * Xpath prefix to lane geometry column header cells
     */
    private static final String LANE_COLUMN_HEADER_CELL_XPATH_PREFIX = "//div[contains(@id, 'laneGeometryTable')]//span[text()='";

    /**
     * Xpath suffix to be used with lane row to find info icon
     */
    private static final String INFO_ICON_XPATH_SUFFIX = ".//img";

    /**
     * Xpath prefix for viewing nodes window header
     */
    private static final String VIEWING_NODES_WINDOW_XPATH_PREFIX = "//div[contains(@class, 'x-title-text')][text()='Viewing Nodes for Lane ";

    /**
     * Xpath suffix for viewing nodes window header
     */
    private static final String VIEWING_NODES_WINDOW_XPATH_SUFFIX = "']/ancestor::div[5]";

    /**
     * Xpath suffix to content area of viewing nodes window( use in conjunction
     * with viewing nodes window xpath)
     */
    private static final String VIEWING_NODES_CONTENT_XPATH_SUFFIX = VIEWING_NODES_WINDOW_XPATH_SUFFIX + "//div[contains(@class, 'x-window-text')]";

    /**
     * Xpath prefix to node cell
     */
    private static final String NODE_CELL_XPATH_PREFIX = ".//span[contains(@data-qtip, '";

    /**
     * Xpath suffix to node cell
     */
    private static final String NODE_CELL_XPATH_SUFFIX = "')]";

    /**
     * Xpath to the Nodes header cell
     */
    private static final String NODES_HEADER_CELL_XPATH = LANE_COLUMN_HEADER_CELL_XPATH_PREFIX + LaneGeometryHeaderCells.NODES.label + COLUMN_HEADER_CELL_XPATH_SUFFIX;

    /**
     * Xpath to the OK button
     */
    private static final String OK_BTN_XPATH = ".//span[text()='OK']";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the lane table
     *
     * @return the lane table element
     */
    private El getLaneTable() {
        return el(By.xpath(LANE_TABLE_XPATH));
    }

    /**
     * Gets the given column header cell
     *
     * @param headerCell -the header cell to get
     * @return the cell element
     */
    private El getHeaderCell(LaneGeometryHeaderCells headerCell) {
        return el(By.xpath(LANE_COLUMN_HEADER_CELL_XPATH_PREFIX + headerCell.label + COLUMN_HEADER_CELL_XPATH_SUFFIX));
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
        waitUntilLoaded();
        El theRow = null;
        List<El> newRows = rows;

        Iterator<El> newIterator = newRows.iterator();
        while (newIterator.hasNext() && theRow == null) {
            El newRow = newIterator.next();

            El firstCell = newRow.el(By.xpath(".//td[contains(@class, 'x-grid-cell-first')]/div"));
            String cellText = firstCell.getText();
            if (cellText.contains(laneID)) {
                theRow = newRow;
                break;
            }
        }
        return theRow;

    }

    /**
     * Gets the info icon for the specified lane ID
     *
     * @param row -the row where info icon should be found
     * @return the icon element
     */
    private El getLaneInfoIcon(El row) {
        return row.el(By.xpath(INFO_ICON_XPATH_SUFFIX));
    }

    /**
     * Gets the Viewing Nodes window for the given lane ID
     *
     * @param laneID -the laneID
     * @return the window element
     */
    private El getViewingNodesWindow(String laneID) {
        return el(By.xpath(VIEWING_NODES_WINDOW_XPATH_PREFIX + laneID + VIEWING_NODES_WINDOW_XPATH_SUFFIX));
    }

    /**
     * Gets the Viewing Nodes content area for the given laneID
     *
     * @param laneID -the laneID
     * @return the content area element
     */
    private El getViewingNodesContentArea(String laneID) {
        return el(By.xpath(VIEWING_NODES_WINDOW_XPATH_PREFIX + laneID + VIEWING_NODES_CONTENT_XPATH_SUFFIX));
    }

    /**
     * Gets the Viewing Nodes content text for the given laneID
     *
     * @param laneID -the laneID
     * @return the content text
     */
    private String getViewingNodesContentText(String laneID) {
        return getViewingNodesContentArea(laneID).getText();
    }

    /**
     * Gets the OK button (in Viewing Nodes window)
     *
     * @param laneID -the laneID of the lane being viewed
     * @return the button element
     */
    private El getOKBtn(String laneID) {
        return getViewingNodesWindow(laneID).el(By.xpath(OK_BTN_XPATH));
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
     * Gets node cell that contains given text in the given row
     *
     * @param row -the row where info is expected
     * @param text -the text expected
     * @return the cell element
     */
    private El getLaneNodeCell(El row, String text) {
        return row.el(By.xpath(NODE_CELL_XPATH_PREFIX + text + NODE_CELL_XPATH_SUFFIX));
    }

    ///////////
    //Element Checkers
    ///////////

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
     * Checks to see if lane rows are displayed
     *
     * @return true if displayed, false if not
     */
    public boolean areLaneRowsDisplayed() {
        try {
            return !(getRows(getLaneTable()).isEmpty());
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if row with given lane Id is displayed in lane table
     *
     * @param laneID -the lane ID expected
     * @param approach -the lane approach
     * @return true if displayed, false if not found or not visible
     */
    public boolean isLaneRowDisplayed(String laneID, String approach) {
        return isElementDisplayed(getLaneRow(laneID, approach));
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
     * Checks if the info icon is displayed for the given lane
     *
     * @param lane -the lane where icon is expected
     * @return true if displayed, false otherwise
     */
    public boolean isLaneInfoIconDisplayed(Lane lane) {
        String laneID = lane.getLaneID().getLabel();
        String type = lane.getLaneType().getLabel();
        El row = getLaneRow(laneID, type);
        return isElementDisplayed(getLaneInfoIcon(row));
    }

    /**
     * Checks if viewing nodes window is displayed for given lane
     *
     * @param laneID -the laneID expected
     * @return true if displayed, false otherwise
     */
    public boolean isViewingNodesWindowDisplayed(String laneID) {
        return isElementDisplayed(getViewingNodesWindow(laneID));
    }

    /**
     * Checks if viewing nodes content area is displayed for given lane
     *
     * @param laneID -the laneID expected
     * @return true if displayed, false otherwise
     */
    public boolean isViewingNodesContentAreaDisplayed(String laneID) {
        return isElementDisplayed(getViewingNodesContentArea(laneID));
    }

    /**
     * Checks if viewing nodes content text is displayed as expected
     *
     * @param laneID -the laneID expected
     * @param text -the text expected
     * @return true if displayed, false otherwise
     */
    public boolean isViewingNodesContentTextDisplayed(String laneID, String text) {
        try {
            String displayedText = getViewingNodesContentText(laneID);
            return displayedText.contains(text);
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks for presence of OK button in Viewing Nodes window
     *
     * @param laneID -the id of the lane being viewed
     * @return true if displayed, false otherwise
     */
    public boolean isLaneOKBtnDisplayed(String laneID) {
        return isElementDisplayed(getOKBtn(laneID));
    }

    /**
     * Checks for presence of OK button in Viewing Nodes window
     *
     * @param lane -the lane being viewed
     * @return true if displayed, false otherwise
     */
    public boolean isLaneOKBtnDisplayed(Lane lane) {
        String laneID = lane.getLaneID().getLabel();
        return isLaneOKBtnDisplayed(laneID);
    }

    /////////////
    // Interaction
    /////////////

    /**
     * Clicks the info icon for the given lane
     *
     * @param lane -the lane where info icon should be clicked
     */
    public void clickInfoIcon(Lane lane) {
        String laneID = lane.getLaneID().getLabel();
        String type = lane.getLaneType().getLabel();
        El row = getLaneRow(laneID, type);
        getLaneInfoIcon(row).click();
        waitForElementToBeVisible(By.xpath(VIEWING_NODES_WINDOW_XPATH_PREFIX + laneID + VIEWING_NODES_WINDOW_XPATH_SUFFIX));
    }

    /**
     * Click OK button
     *
     * @param laneID -the laneID of the lane being viewed
     */
    public void clickOKBtn(String laneID) {
        getOKBtn(laneID).click();
    }

    /**
     * Click OK button
     *
     * @param lane -the lane being viewed
     */
    public void clickOKBtn(Lane lane) {
        String laneID = lane.getLaneID().getLabel();
        clickOKBtn(laneID);
    }

    /////////////
    // Utilities
    /////////////

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
     * Checks that viewing nodes associated with the given lane display in the
     * Viewing Nodes window for the given lane
     *
     * @param lane -the lane to check
     */
    public void checkViewingNodesContent(Lane lane) {
        String laneID = lane.getLaneID().getLabel();
        String startNode = lane.getLaneStartNode();
        String endNode = lane.getLaneEndNode();
        Assert.assertTrue("Could not find expected starting node of, '" + startNode + "', in Viewing Nodes window for lane with ID " + laneID + ".",
                isViewingNodesContentTextDisplayed(laneID, startNode));
        Assert.assertTrue("Could not find expected ending node of, '" + endNode + "', in Viewing Nodes window for lane with ID " + laneID + ".", isViewingNodesContentTextDisplayed(laneID, endNode));

    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(NODES_HEADER_CELL_XPATH));
    }
}
