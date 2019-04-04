package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import java.io.File;
import java.io.IOException;

import junit.framework.Assert;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.LocalFileDetector;
import org.openqa.selenium.remote.RemoteWebElement;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationType;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Upload Simulation modal, displayed when you
 * select Create > Upload
 *
 * @author llaroussini
 */
public class CreateSimulationFromUploadModal extends CreateSimulationModal {

    /**
     * Default constructor
     *
     * @param driver the web driver object being used
     */
    public CreateSimulationFromUploadModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the create simulation from upload header
     */
    private static final String CREATE_SIM_UPLOAD_HEADER_TEXT = "Create Simulation from Upload";

    /**
     * Text displayed in the create simulation from upload help header
     */
    private static final String CREATE_SIM_UPLOAD_HELP_HEADER_TEXT = "Create Simulation from Upload Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Create a new simulation with the specified composite, name, and type using the selected simulation (.zip) file. The simulation name must be unique in the composite. Entering the name of a composite that does not exist will create a new composite with the specified name to hold the created simulation.";

    /**
     * Text expected to be displayed in the name field by default.
     */
    public static final String DEFAULT_NAME_TEXT = "";

    /**
     * Text expected to be displayed in the Upload field by default
     */
    public static final String DEFAULT_UPLOAD_TEXT = "Select a file to upload";

    /**
     * Xpath prefix to all buttons
     */
    private static final String CREATE_UPLOADED_SIM_FORM_BTN_XPATH_PREFIX = "//div[contains(@id, 'simulation')][contains(@id, 'Upload')]//span[text()='";

    /**
     * Xpath to the Select button
     */
    private static final String SELECT_BTN_XPATH = CREATE_UPLOADED_SIM_FORM_BTN_XPATH_PREFIX + "Select" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Create button
     */
    private static final String CREATE_BTN_XPATH = CREATE_UPLOADED_SIM_FORM_BTN_XPATH_PREFIX + "Create" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Reset button
     */
    private static final String RESET_BTN_XPATH = CREATE_UPLOADED_SIM_FORM_BTN_XPATH_PREFIX + "Reset" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Cancel button
     */
    private static final String CANCEL_BTN_XPATH = CREATE_UPLOADED_SIM_FORM_BTN_XPATH_PREFIX + "Cancel" + BTN_XPATH_SUFFIX;

    /**
     * By locator suitable for locating the 'Upload' textbox which displays the
     * file name/path
     */
    private static final By UPLOAD_LOCATOR = By.xpath(".//input[contains(@id, 'simulation')][contains(@id, 'Upload')][contains(@class, 'x-form-field')]");

    /**
     * By Locator suitable for locating the Texas radio button in the Traffic
     * Simulator selection
     */
    private static final By TEXAS_RADIO_LOCATOR = By.xpath(".//label[contains(@id, 'boxLabelEl')][text()='TEXAS']");

    /**
     * By locator suitable for locating the Playback radio button in the traffic
     * simulator selection
     */
    private static final By PLAYBACK_RADIO_LOCATOR = By.xpath(".//label[contains(@id, 'boxLabelEl')][text()='Playback']");

    /**
     * Xpath to the Browse button
     */
    private static final String BROWSE_BTN_XPATH = "//div[contains(@id, 'Upload')]//input[contains(@id, 'button-fileInputEl')]";

    /**
     * By locator suitable for locating the pop-up which displays as a file is
     * uploading
     */
    private static final By UPLOAD_FILE_POPUP_LOCATOR = By.xpath(".//div[contains(@id,'messagebox')][contains(text(),'Uploading file...')]");

    /**
     * Upload field name as displayed in UI
     */
    private static final String UPLOAD_FIELD_DISPLAYED_NAME = "Upload";

    /**
     * Error text displayed with icon/tooltip when invalid file is uploaded
     */
    private static final String INVALID_FILE_ERROR_TEXT = "A valid simulation (.zip) file is required.";

    /**
     * Xpath to Simulation Failed error pop-up
     */
    private static final String SIMULATION_FAILED_ERROR_XPATH = "//div[contains(text(), 'Please upload a zip file of a simulation')]";

    /**
     * Xpath to OK btn in Simulation Failed error pop-up
     */
    private static final String SIM_FAILED_OK_BTN_XPATH = SIMULATION_FAILED_ERROR_XPATH + "/ancestor::div//span[text()='OK']";

    /**
     * Xpath to server invalid error pop-up
     */
    private static final String SERVER_INVALID_ERROR_POP_UP_XPATH = "//div[text()='SERVER_INVALID']/ancestor::div[contains(@id, 'messagebox')]";

    /**
     * XPath to server invalid error OK button
     */
    private static final String SERVER_INVALID_ERROR_OK_BTN = SERVER_INVALID_ERROR_POP_UP_XPATH + "//span[contains(@id, 'button')]";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the upload form content window to the help icon
     *
     * @return the help icon
     */
    private El getUploadSimHelpIcon() {
        return getHelpIcon(CREATE_SIM_UPLOAD_HEADER_TEXT);
    }

    /**
     * Gets the select button
     *
     * @return the select button
     */
    private El getSelectBtn() {
        return el(By.xpath(SELECT_BTN_XPATH));
    }

    /**
     * Gets the create button
     *
     * @return the create button
     */
    private El getCreateBtn() {
        return el(By.xpath(CREATE_BTN_XPATH));
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return el(By.xpath(RESET_BTN_XPATH));
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return el(By.xpath(CANCEL_BTN_XPATH));
    }

    /**
     * Gets the Browse button
     *
     * @return the button element
     */
    private El getBrowseBtn() {
        return el(By.xpath(BROWSE_BTN_XPATH));
    }

    /**
     * Gets the upload field (textbox)
     *
     * @return the upload filename/path textbox element
     */
    private El getUploadField() {
        return el(UPLOAD_LOCATOR);
    }

    /**
     * Gets the Texas radio button selector for the Traffic Simulator field
     *
     * @return the Texas radio button
     */
    private El getTexasRadio() {
        return el(TEXAS_RADIO_LOCATOR);
    }

    /**
     * Gets the Playback radio button selector for the Traffic Simulator field
     *
     * @return the Playback radio button
     */
    private El getPlaybackRadio() {
        return el(PLAYBACK_RADIO_LOCATOR);
    }

    /**
     * Gets the Simulation Failed error pop up
     *
     * @return the Simulation Failer error pop up
     */
    private El getSimulationFailureErrorPopUp() {
        return el(By.xpath(SIMULATION_FAILED_ERROR_XPATH));
    }

    /**
     * Gets the OK button displayed in Simulation Failed error pop up
     *
     * @return the OK button element
     */
    private El getSimFailedOKBtn() {
        return el(By.xpath(SIM_FAILED_OK_BTN_XPATH));
    }

    /**
     * Gets the Sever Failed error pop up
     *
     * @return the Server Failed error pop up
     */
    private El getServerFailureErrorPopUp() {
        return el(By.xpath(SERVER_INVALID_ERROR_POP_UP_XPATH));
    }

    /**
     * Gets the OK button displayed in Server Failed error pop up
     *
     * @return the OK button element
     */
    private El getServerFailedOKBtn() {
        return el(By.xpath(SERVER_INVALID_ERROR_OK_BTN));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the create simulation from upload header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateSimUploadHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_SIM_UPLOAD_HEADER_TEXT);
    }

    /**
     * Checks to see if the create simulation from upload help header is
     * displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateSimUploadHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_SIM_UPLOAD_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isUploadSimHelpIconDisplayed() {
        return isHelpIconDisplayed(CREATE_SIM_UPLOAD_HEADER_TEXT);
    }

    /**
     * Checks to see if the select button is displayed
     *
     * @return true if the select button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isSelectBtnDisplayed() {
        return isElementDisplayed(getSelectBtn());
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
     * Checks to see if the upload field is displayed
     *
     * @return true if the upload field is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isUploadFieldDisplayed() {
        return isElementDisplayed(UPLOAD_LOCATOR);
    }

    /**
     * Checks to see if the browse button is displayed.
     *
     * @return true if the browse button is displayed, false it is not or cannot
     *         be found
     */
    public boolean isBrowseBtnDisplayed() {
        return getBrowseBtn() != null;
    }

    /**
     * Checks to see if the texas radio button is displayed
     *
     * @return true if the texas radio button is displayed, false if it is not
     *         or cannot be found
     */
    public boolean isTexasRadioDisplayed() {
        return isElementDisplayed(TEXAS_RADIO_LOCATOR);
    }

    /**
     * Checks to see if the playback radio button is displayed
     *
     * @return true if the playback button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isPlaybackRadioDisplayed() {
        return isElementDisplayed(PLAYBACK_RADIO_LOCATOR);
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
        return isHelpOKBtnDisplayed(CREATE_SIM_UPLOAD_HELP_HEADER_TEXT);
    }

    /**
     * Checks if Upload field required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isUploadRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(UPLOAD_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks to see if Invalid File Upload error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidFileUploadErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_FILE_ERROR_TEXT));
    }

    /**
     * Checks to see if Simulation failure error pop-up is displayed
     *
     * @return true if the error pop-up is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isSimualtionFailureErrorDisplayed() {
        return isElementDisplayed(getSimulationFailureErrorPopUp());
    }

    /**
     * Checks to see if Server failure error pop-up is displayed
     *
     * @return true if the error pop-up is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isServerFailureErrorDisplayed() {
        return isElementDisplayed(getServerFailureErrorPopUp());
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Clicks the Select button
     */
    public void clickSelect() {
        getSelectBtn().click();
    }

    /**
     * Selects the given composite (handles clicking the select button as well)
     *
     * @param compositeName -the name of the composite to select
     */
    public void selectComposite(String compositeName) {
        clickSelect();
        waitForElementToBeVisible(By.xpath(COMPOSITE_OPTIONS_LIST_XPATH));
        getSpecificCompositeOption(compositeName).click();
    }

    /**
     * Selects the given composite (handles clicking the select button as well)
     *
     * @param composite -the composite to select
     */
    public void selectComposite(CompositeSimulation composite) {
        String compositeName = composite.getName();
        selectComposite(compositeName);
    }

    /**
     * Clicks the browse button
     */
    public void clickBrowseBtn() {
        getBrowseBtn().click();
    }

    /**
     * Click the Create button
     *
     * @param success - true if success is expected, false otherwise
     * @return the newly loaded Simulation Page if success expected, no return
     *         otherwise
     */
    public SimulationsPage clickCreate(boolean success) {
        getCreateBtn().click();
        if (success) {
            waitUntilAlertIsPresent(settingUpAlert(), 20);
            waitUntilAlertIsNotPresent(settingUpAlert(), 20);
            return getPage(SimulationsPage.class);
        }
        else {
            return null;
        }
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
     * @return the newly loaded/updated simulations page
     */
    public SimulationsPage clickCancel() {
        getCancelBtn().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Click the Upload Sim Help icon
     */
    public void clickUploadSimHelp() {
        getUploadSimHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_SIM_UPLOAD_HELP_HEADER_TEXT);
    }

    /**
     * Clicks the Close icon associated with the Create Simulation from Upload
     * modal
     *
     * @return the newly loaded SimulationsPage
     */
    public SimulationsPage clickCloseIcon() {
        clickCloseIcon(CREATE_SIM_UPLOAD_HEADER_TEXT);
        return getPage(SimulationsPage.class);
    }

    /**
     * Gets the text displayed in the upload field (i.e. the currently selected
     * file path)
     *
     * @return the text displayed in the upload field
     */
    public String getUploadFieldText() {
        return getUploadField().getText();
    }

    /**
     * Attempt at setting the upload file, based off NavyPal's implementation
     *
     * @param file the file to be set
     * @throws IOException if an error occurs getting the file path, likely the
     *         result of a permissions error
     */
    public void setUploadFile(File file) throws IOException {
        String path = file.getCanonicalPath();
        El browseBtn = getBrowseBtn();
        if (IS_REMOTE) {
            RemoteWebElement uploader = ((RemoteWebElement)browseBtn.webElement()); //TODO implement in automation base - El
            uploader.setFileDetector(new LocalFileDetector());
            uploader.sendKeys(path);
        }
        else {
            browseBtn.sendKeys(path);
        }
        ETexasCommonUtils.sleep(2000); //ZZZ - allows time for file to display - unable to access displayed text in field to check that file name is displayed
    }

    /**
     * Clicks the Texas radio button
     */
    public void clickTexasRadio() {
        getTexasRadio().click();
    }

    /**
     * Clicks the Playback radio button
     */
    public void clickPlayackRadio() {
        getPlaybackRadio().click();
    }

    /**
     * Clicks the OK button in Simulation Failure pop-up
     */
    public void clickOKSimError() {
        getSimFailedOKBtn().click();
    }

    /**
     * Clicks the OK button in Server Failure pop-up
     */
    public void clickOKServerError() {
        getServerFailedOKBtn().click();
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for presence of Create Simulation from Upload header
     */
    public void checkUploadSimHeader() {
        Assert.assertTrue("Create Simulation from Upload header is not displayed.", isCreateSimUploadHeaderDisplayed());
    }

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkUploadSimHeaderIcons() {
        checkHeaderIcons(CREATE_SIM_UPLOAD_HEADER_TEXT);
    }

    /**
     * Verifies the Create Simulation from Upload Help modal is displayed
     * (checks header and content text)
     */
    public void checkHelpModal() {
        Assert.assertTrue("The Create Simulation from Template Help header is not displayed.", isCreateSimUploadHelpHeaderDisplayed());
        Assert.assertTrue("The Create Simulation from Template Help content is not displayed.", isHelpContentDisplayed());
    }

    /**
     * Checks to see that all elements in the form are correctly displayed
     * (Composite text box, Composite select button, Simulation text box, Upload
     * text box, Browse button, TEXAS radio button, Playback radio butto)
     */
    public void checkFields() {
        Assert.assertTrue("Composite Name field is not displayed on the upload simulation form.", isCompositeNameTextBoxDisplayed());
        Assert.assertTrue("Composite select button is not displayed on the upload simulation form.", isSelectBtnDisplayed());
        Assert.assertTrue("Simulation Name field is not displayed on the upload simulation form.", isSimNameTextBoxDisplayed());
        Assert.assertTrue("Upload field is not displayed on the upload simulation form.", isUploadFieldDisplayed());
        Assert.assertTrue("Browse button is not displayed on the upload simulation form.", isBrowseBtnDisplayed());
        Assert.assertTrue("Texas radio button is not displayed on the upload simulation form.", isTexasRadioDisplayed());
        Assert.assertTrue("Playback radio button is not displayed on the upload simulation form.", isPlaybackRadioDisplayed());
    }

    /**
     * Checks to see that all buttons expected at the bottom of the modal are
     * displayed (Create, Reset, and Cancel)
     */
    public void checkBtns() {
        Assert.assertTrue("Create button is not displayed on the upload simulation form.", isCreateBtnDisplayed());
        Assert.assertTrue("Reset button is not displayed on the upload simulation form.", isResetBtnDisplayed());
        Assert.assertTrue("Cancel button is not displayed on the upload simulation form.", isCancelBtnDisplayed());
    }

    /**
     * Checks that the upload file dialog is displayed, then waits until it
     * disappears
     */
    public void checkUploadDialogAndWait() {
        waitUntilAlertIsPresent(UPLOAD_FILE_POPUP_LOCATOR, PAGE_LOAD_TIMEOUT);
        waitUntilAlertIsNotPresent(UPLOAD_FILE_POPUP_LOCATOR, 120);
    }

    /**
     * Verifies the composite options list is displayed
     */
    public void checkCompositeList() {
        clickSelect();
        Assert.assertNotNull("The composite list could not be found after clicking the select button.", getCompositeOptionsList());
        clickSelect();
        waitForElementToBeInvisible(By.xpath(COMPOSITE_OPTIONS_LIST_XPATH));
    }

    /**
     * Checks that displayed uploaded file name matches expected file name
     *
     * @param name -the uploaded file name expected
     */
    public void checkUploadField(String name) {
        Assert.assertEquals("Displayed uploaded file name does not match expected file name.", name, getUploadFieldText());
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Upload text box
     */
    public void checkUploadFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Upload text box.", isFieldRequiredErrorDisplayed(UPLOAD_FIELD_DISPLAYED_NAME));
    }

    /**
     * Sets all fields in Create Simulation from Upload modal based on given
     * simulation and file
     *
     * @param sim -the uploaded simulation to create
     * @param file -the file to use
     * @throws IOException if an error occurs getting the file path, likely the
     *         result of a permissions error
     */
    public void setAllFields(UploadedSimulation sim, File file) throws IOException {
        setCompositeName(sim);
        setSimName(sim);
        setUploadFile(file);
        if (sim.getSimType() == SimulationType.ETEXAS) {
            clickTexasRadio();
        }
        else {
            clickPlayackRadio();
        }
    }

    ////////
    // Waits
    ///////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(TEXAS_RADIO_LOCATOR, 30);
    }
}
