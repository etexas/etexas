package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CellTower;
import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ETexasBasePage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Confirm Delete modal
 *
 * @author llaroussini
 */
public class ConfirmDeleteModal extends ETexasBasePage {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public ConfirmDeleteModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Button options in the Confirm Delete modal
     *
     * @author llaroussini
     */
    public enum Btn {
        /**
         * The Yes button
         */
        YES("Yes"),
        /**
         * The No button
         */
        NO("No");

        /**
         * The label of the option links as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        Btn(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the button as it is displayed in the
         * Web UI
         *
         * @return The label of the button
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * The xpath prefix assigned to the confirm delete header
     */
    private static final String TITLE_XPATH = "//div[contains(@class, 'x-title-text')][@data-ref='textEl'][ starts-with(text(), 'Delete') and contains(text(), 'Application')]";

    /**
     * The text displayed in the header
     */
    private static final String HEADER_TEXT = "Delete Native Application";

    /**
     * Xpath prefix assigned to buttons in the form
     */
    private static final String FORM_BTN_XPATH_PREFIX = ".//span[@data-ref='btnInnerEl'][text()='";

    /**
     * XPath suffix
     */
    private static final String XPATH_SUFFIX = "']";

    /**
     * Xpath prefix assigned to content are in help window
     */
    private static final String CONTENT_AREA_XPATH_PREFIX = "//div[contains(@class, 'x-component-default')][text()='";

    /**
     * Text displayed as the Delete Warning simulation content
     */
    private static final String DELETE_WARNING_SIMULATION_CONTENT_TEXT = "Are you sure you want to delete the selected simulation?";

    /**
     * Text displayed as the Delete Warning execution content
     */
    private static final String DELETE_WARNING_EXECUTION_CONTENT_TEXT = "Are you sure you want to delete the selected execution?";

    /**
     * Text displayed as the Delete Warning remote app content
     */
    private static final String DELETE_WARNING_REMOTE_APP_CONTENT_TEXT = "Are you sure you want to delete the selected remote app?";

    /**
     * Text displayed as the Delete Warning native app content
     */
    private static final String DELETE_WARNING_JAR_APP_CONTENT_TEXT = "You are about to delete the following jar name and its apps which may affect your simulations and executions:";

    /**
     * Text displayed as the Delete Warning app content prefix
     */
    private static final String DELETE_WARNING_APP_CONTENT_TEXT_PREFIX = "Deleting \"";

    /**
     * Text displayed as the Delete Warning app content suffix
     */
    private static final String DELETE_WARNING_APP_CONTENT_TEXT_SUFFIX = "\" will affect any executions with instances of the application. Are you sure you want to delete it?";

    /**
     * Text displayed as the Delete Warning detector content
     */
    private static final String DELETE_WARNING_DETECTOR_CONTENT_TEXT_PREFIX = "Deleting detector ";

    /**
     * Text displayed as the Delete Warning device content
     */
    private static final String DELETE_WARNING_DEVICE_CONTENT_TEXT = "Are you sure you want to delete the selected device?";

    /**
     * Generic text prefix displayed as the Delete Warning RSE content
     */
    private static final String GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX = "Deleting \"";

    /**
     * Text prefix displayed as the Delete Warning cell tower content
     */
    private static final String DELETE_WARNING_CELL_TOWER_CONTENT_TEXT_PREFIX = "Deleting cell tower ";

    /**
     * Generic text suffix displayed as part of all Delete Warning content
     */
    private static final String GENERIC_DELETE_WARNING_CONTENT_TEXT_SUFFIX = "\" cannot be undone. Are you sure you want to delete it?";

    /**
     * Text suffix displayed as the Delete Warning composite content
     */
    private static final String DELETE_WARNING_COMPOSITE_CONTENT_TEXT_SUFFIX = "\" will result in the loss of all simulations associated with the composite. Are you sure you want to delete it?";

    /**
     * Text suffix displayed as the Delete Warning simulation content
     */
    private static final String DELETE_WARNING_SIMULATION_CONTENT_TEXT_SUFFIX = "\" will result in the loss of all cell towers, detectors, devices, and lane mappings associated with the simulation. Are you sure you want to delete it?";

    /**
     * Text suffix displayed as the Delete Warning detector content
     */
    private static final String DELETE_WARNING_DEVICE_CONTENT_TEXT_SUFFIX = " cannot be undone. Are you sure you want to delete it?";

    /**
     * Mid-text displayed as the Delete Warning JAR app content suffix
     */
    private static final String DELETE_WARNING_JAR_CONTENT_MID_TEXT = "\" will delete all applications in the \"";

    /**
     * Text suffix displayed as the Delete Warning JAR app content suffix
     */
    private static final String DELETE_WARNING_JAR_CONTENT_TEXT_SUFFIX = "\" will affect any executions with instances of the application. All JAR applications uploaded from the same file will also be removed. Are you sure you want to delete it?";

    /**
     * Xpath to get to the full Form Header of a window from an interior element
     */
    private static final String FORM_WINDOW_PARENT_XPATH = "/ancestor::div[contains(@class, 'x-border-box')]";

    /**
     * Xpath to Close icon in Confirm Delete modal
     */
    private static final String CLOSE_ICON_XPATH = "//div[contains(@id, 'messagebox')]//div[@data-qtip='Close dialog']";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the header element
     *
     * @return the header element for the form
     */
    private El getHeader() {
        return el(By.xpath(TITLE_XPATH));
    }

    /**
     * Gets button with given text in form with given header text
     *
     * @param btnName -the text displayed on the button in the UI
     * @param headerText -the text in the header of the window where the button
     *        is expected
     * @return the button element
     */
    private El getFormBtn(String headerText, String btnName) {
        El formWindow = el(By.xpath(TITLE_XPATH + FORM_WINDOW_PARENT_XPATH));
        El btn = el(formWindow.el(By.xpath(FORM_BTN_XPATH_PREFIX + btnName + XPATH_SUFFIX)));
        return btn;
    }

    /**
     * Gets the content area element
     *
     * @param text - the text expected in the content area
     * @return the content area element for the form
     */
    private El getContentArea(String text) {
        return el(By.xpath(CONTENT_AREA_XPATH_PREFIX + text + XPATH_SUFFIX));
    }

    /**
     * Gets the Close icon
     *
     * @return the close icon element
     */
    private El getCloseIcon() {
        return el(By.xpath(CLOSE_ICON_XPATH));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the confirm delete header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isConfirmDeleteHeaderDisplayed() {
        return isElementDisplayed(getHeader());
    }

    /**
     * Checks to see if the delete warning content for deletion of a composite
     * is displayed
     *
     * @param composite the composite being deleted
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCompositeDeletionContentDisplayed(CompositeSimulation composite) {
        String compositeName = composite.getName();
        String text = GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX + compositeName + DELETE_WARNING_COMPOSITE_CONTENT_TEXT_SUFFIX;
        return isElementDisplayed(getContentArea(GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX + compositeName + DELETE_WARNING_COMPOSITE_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of a simulation
     * is displayed
     *
     * @param sim the simulation being deleted
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isSimulationDeletionContentDisplayed(Simulation sim) {
        String simName = sim.getName();
        return isElementDisplayed(getContentArea(GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX + simName + DELETE_WARNING_SIMULATION_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of an execution
     * is displayed
     *
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isExecutionDeletionContentDisplayed() {
        return isElementDisplayed(getContentArea(DELETE_WARNING_EXECUTION_CONTENT_TEXT));
    }

    /**
     * Checks to see if the delete warning content for deletion of a remote app
     * is displayed
     *
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isRemoteAppDeletionContentDisplayed(UserRemoteApp remoteApp) {
        return isElementDisplayed(getContentArea(DELETE_WARNING_APP_CONTENT_TEXT_PREFIX + remoteApp.getName() + DELETE_WARNING_APP_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of a native app
     * is displayed
     *
     * @param nativeApp - the nativeApp being deleted
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isNativeAppDeletionContentDisplayed(UserNativeApp nativeApp) {
        return isElementDisplayed(getContentArea(DELETE_WARNING_APP_CONTENT_TEXT_PREFIX + nativeApp.getName() + DELETE_WARNING_APP_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of a jar app is
     * displayed
     *
     * @param jarName - the name of the JAR being deleted
     * @param fileName -the name of the app file being deleted
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isJarAppDeletionContentDisplayed(String jarName, String fileName) {
        return isElementDisplayed(getContentArea(DELETE_WARNING_APP_CONTENT_TEXT_PREFIX + fileName + DELETE_WARNING_JAR_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of a detector is
     * displayed
     *
     * @param detector - the detector being deleted
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isDetectorDeletionContentDisplayed(Detector detector) {
        return isElementDisplayed(getContentArea(DELETE_WARNING_DETECTOR_CONTENT_TEXT_PREFIX + detector.getID() + DELETE_WARNING_DEVICE_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of a fixed
     * cellular is displayed
     *
     * @param fixedcell - the fixed cell being deleted
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isFixedCellularDeviceDeletionContentDisplayed(FixedCellularDevice fixedcell) {
        return isElementDisplayed(getContentArea(GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX + fixedcell.getName() + GENERIC_DELETE_WARNING_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of a device is
     * displayed
     *
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isDeviceDeletionContentDisplayed() {
        return isElementDisplayed(getContentArea(DELETE_WARNING_DEVICE_CONTENT_TEXT));
    }

    /**
     * Checks to see if the delete warning content for deletion of a cell tower
     * is displayed
     *
     * @param tower -the cell tower being deleted
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCellTowerDeletionContentDisplayed(CellTower tower) {
        return isElementDisplayed(getContentArea(DELETE_WARNING_CELL_TOWER_CONTENT_TEXT_PREFIX + tower.getID() + DELETE_WARNING_DEVICE_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of an RSE device
     * is displayed
     *
     * @param rse -the RSE device being deleted
     * @return true if the content is displayed, false otherwise
     */
    public boolean isRSEDeletionContentDisplayed(RSEDevice rse) {
        return isElementDisplayed(getContentArea(GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX + rse.getName() + GENERIC_DELETE_WARNING_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of an OBU device
     * is displayed
     *
     * @param obu -the OBU device being deleted
     * @return true if the content is displayed, false otherwise
     */
    public boolean isOBUDeletionContentDisplayed(OBUDevice obu) {
        return isElementDisplayed(getContentArea(GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX + obu.getName() + GENERIC_DELETE_WARNING_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the delete warning content for deletion of a Cellular
     * device is displayed
     *
     * @param device -the Cellular device being deleted
     * @return true if the content is displayed, false otherwise
     */
    public boolean isCellularDeviceDeletionContentDisplayed(CellularDevice device) {
        return isElementDisplayed(getContentArea(GENERIC_DELETE_WARNING_CONTENT_TEXT_PREFIX + device.getName() + GENERIC_DELETE_WARNING_CONTENT_TEXT_SUFFIX));
    }

    /**
     * Checks to see if the given button is displayed
     *
     * @param btn -the button expected
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isBtnDisplayed(Btn btn) {
        String btnName = btn.getLabel();
        return isElementDisplayed(getFormBtn(HEADER_TEXT, btnName));
    }

    /**
     * Checks if Close icon is displayed in Delete Warning modal
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCloseIconDisplayed() {
        return isElementDisplayed(getCloseIcon());
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Checks for the presence of the Confirm Delete header
     */
    public void checkConfirmDeleteHeader() {
        Assert.assertTrue("The header is not displayed as expected in the Confirm Delete window.", isConfirmDeleteHeaderDisplayed());
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * composite
     *
     * @param composite the composite being deleted
     */
    public void checkDeleteWarningCompositeContent(CompositeSimulation composite) {
        Assert.assertTrue("Content is not displayed as expected in the Composite Delete Warning window.", isCompositeDeletionContentDisplayed(composite));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * simulation
     *
     * @param sim the simulation being deleted
     */
    public void checkDeleteWarningSimulationContent(Simulation sim) {
        Assert.assertTrue("Content is not displayed as expected in the Simulation Delete Warning window.", isSimulationDeletionContentDisplayed(sim));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting an
     * execution
     */
    public void checkDeleteWarningExecutionContent() {
        Assert.assertTrue("Content is not displayed as expected in the Exeuction Delete Warning window.", isExecutionDeletionContentDisplayed());
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * remote app
     */
    public void checkDeleteWarningRemoteAppContent(UserRemoteApp remoteApp) {
        Assert.assertTrue("Content is not displayed as expected in the Remote App Delete Warning window.", isRemoteAppDeletionContentDisplayed(remoteApp));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * native app
     *
     * @param nativeApp - the native app being deleted
     */
    public void checkDeleteWarningNativeAppContent(UserNativeApp nativeApp) {
        Assert.assertTrue("Content is not displayed as expected in the Native App Delete Warning window.", isNativeAppDeletionContentDisplayed(nativeApp));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a jar
     * app
     *
     * @param jarName - the name of the JAR being deleted
     * @param appName -the name of the application within the JAR being deleted
     */
    public void checkDeleteWarningJarAppContent(String jarName, String appName) {
        Assert.assertTrue("Content is not displayed as expected in the JAR App Delete Warning window.", isJarAppDeletionContentDisplayed(jarName, appName));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * detector
     *
     * @param detector - the detector being deleted
     */
    public void checkDeleteWarningDetectorContent(Detector detector) {
        Assert.assertTrue("Content is not displayed as expected in the Detector Delete Warning window.", isDetectorDeletionContentDisplayed(detector));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * fixed cellular device
     *
     * @param fixedcell - the fixed cellular device being deleted
     */
    public void checkDeleteWarningFixedCellularDeviceContent(FixedCellularDevice fixedcell) {
        Assert.assertTrue("Content is not displayed as expected in the Fixed Cellular Device Delete Warning window.", isFixedCellularDeviceDeletionContentDisplayed(fixedcell));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * device
     */
    public void checkDeleteWarningDeviceContent() {
        Assert.assertTrue("Content is not displayed as expected in the Device Delete Warning window.", isDeviceDeletionContentDisplayed());
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * cell tower
     *
     * @param tower -the cell tower being deleted
     */
    public void checkDeleteWarningCellTowerContent(CellTower tower) {
        Assert.assertTrue("Content is not displayed as expected in the Confirm Delete modal when deleting a cell tower.", isCellTowerDeletionContentDisplayed(tower));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting a
     * road side equipment device
     *
     * @param rse -the RSE device being deleted
     */
    public void checkDeleteWarningRSEContent(RSEDevice rse) {
        Assert.assertTrue("Content is not displayed as expected in the Confirm Delete modal when deleting a road side equipment device.", isRSEDeletionContentDisplayed(rse));
    }

    /**
     * Checks for the presence of the Delete Warning content when deleting an on
     * board unit device profile
     *
     * @param obu -the OBU device being deleted
     */
    public void checkDeleteWarningOBUContent(OBUDevice obu) {
        Assert.assertTrue("Content is not displayed as expected in the Confirm Delete modal when deleting an on board unit device profile.", isOBUDeletionContentDisplayed(obu));
    }

    /**
     * Checks for presence of Yes and No buttons in confirm delete modal
     */
    public void checkConfirmDeleteBtns() {
        Assert.assertTrue("Yes button not displayed in Delete Warning window.", isBtnDisplayed(Btn.YES));
        Assert.assertTrue("No button not displayed in Delete Warning window.", isBtnDisplayed(Btn.NO));
    }

    /**
     * Clicks the given button
     *
     * @param button -the button to click
     */
    public void clickBtn(Btn button) {
        String btnName = button.getLabel();
        getFormBtn(HEADER_TEXT, btnName).click();
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH));
        ETexasCommonUtils.sleep(1000); //ZZZ - allows time for deleted item to no longer be visible in UI
    }

    /**
     * Clicks the Close icon in the Confirm Delete modal
     */
    public void clickCloseIcon() {
        getCloseIcon().click();
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TITLE_XPATH));
    }

}
