package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Copy Simulation modal
 *
 * @author llaroussini
 */
public class CopySimulationModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CopySimulationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the copy simulation header
     */
    private static final String COPY_SIM_HEADER_TEXT = "Copy Simulation";

    /**
     * Text displayed in the copy simulation help header
     */
    private static final String COPY_SIM_HELP_HEADER_TEXT = "Copy Simulation Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Copy the selected simulation. The simulation name must be unique in the composite.";

    /**
     * Text associated with copying simulation alert
     */
    private static final String COPYING_SIM_ALERT_TEXT = "Copying simulation...";

    /**
     * The xpath of the Composite Name text box
     */
    private static final String COMPOSITE_NAME_TEXT_BOX_XPATH = "//input[@name='compositeName']";

    /**
     * The xpath of the Simulation Name text box
     */
    private static final String SIMULATION_NAME_TEXT_BOX_XPATH = "//input[@name='simulationName']";

    /**
     * Xpath prefix to all buttons
     */
    private static final String COPY_SIM_FORM_BTN_XPATH_PREFIX = "//div[contains(@id, 'simulation')][contains(@id, 'copy')]//span[text()='";

    /**
     * Xpath suffix to all buttons
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to the Select button
     */
    private static final String SELECT_BTN_XPATH = COPY_SIM_FORM_BTN_XPATH_PREFIX + "Select" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Create button
     */
    private static final String CREATE_BTN_XPATH = COPY_SIM_FORM_BTN_XPATH_PREFIX + BtnNames.CREATE.getLabel() + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Reset button
     */
    private static final String RESET_BTN_XPATH = COPY_SIM_FORM_BTN_XPATH_PREFIX + BtnNames.RESET.getLabel() + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Cancel button
     */
    private static final String CANCEL_BTN_XPATH = COPY_SIM_FORM_BTN_XPATH_PREFIX + BtnNames.CANCEL.getLabel() + BTN_XPATH_SUFFIX;

    /**
     * The xpath of the composite options list
     */
    private static final String COMPOSITE_OPTIONS_LIST_XPATH = "//div[contains(@id, 'menu')][@aria-expanded='true']";

    /**
     * Xpath prefix to specific composite option in options list
     */
    private static final String SPECIFIC_COMPOSITE_OPTION_XPATH_PREFIX = COMPOSITE_OPTIONS_LIST_XPATH + "//span[text()='";

    /**
     * Composite name field label as displayed in UI
     */
    private static final String COMPOSITE_NAME_FIELD_LABEL = "Composite";

    /**
     * Simulation name field label as displayed in UI
     */
    private static final String SIMULATION_NAME_FIELD_LABEL = "Name";

    /**
     * Xpath prefix to the data-errorqtip field used for invalid Simulation
     * names
     */
    private static final String TOOLTIP_ERROR_SIM_PREFIX = "//div[contains(@id, 'ETexas-simulation-view-copy-simulation-name-field')][contains(@data-errorqtip, '";

    /**
     * Error text displayed with icon/tooltip when invalid sim name is used
     */
    private static final String INVALID_SIMULATION_NAME_ERROR_TEXT = "Simulation names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Error text displayed with icon/tooltip when leading/trailing whitespace
     * is used in sim name
     */
    private static final String LEADING_TRAILING_WHITESPACE_SIMULATION_NAME_ERROR_TEXT = "Simulation names may not contain leading or trailing spaces.";

    /**
     * Error text displayed with icon/tooltip when a simulation requires a valid
     * simulation name
     */
    private static final String SIMULATION_NAME_REQUIRED_TEXT = "A valid simulation name is required.";

    /**
     * Error text displayed when having the same simulation and composite name
     */
    private static final String SIMULATION_COMPOSITE_SAME_NAME_ERROR = "A simulation cannot have the same name as the composite";

    /**
     * Error text prefix displayed when two Simulations have the same name in a
     * Composite
     */
    private static final String IDENTICAL_SIM_NAME_ERROR_PREFIX = "A simulation with the name \"";

    /**
     * Error text suffix displayed when two Simulations have the same name in a
     * Composite
     */
    private static final String IDENTICAL_SIME_NAME_ERROR_SUFFIX = "\" already exists in the composite.";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the copy simulation header to the help icon
     *
     * @return the help icon
     */
    private El getCopySimHelpIcon() {
        return getHelpIcon(COPY_SIM_HEADER_TEXT);
    }

    /**
     * Gets the Composite Name text box
     *
     * @return the text box element
     */
    private El getCompositeNameTextBox() {
        return el(By.xpath(COMPOSITE_NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Simulation Name text box
     *
     * @return the text box element
     */
    private El getSimulationNameTextBox() {
        return el(By.xpath(SIMULATION_NAME_TEXT_BOX_XPATH));
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
     * Gets the composite option with the given composite name
     *
     * @param compositeName -the name of the composite
     * @return the option element
     */
    private El getSpecificCompositeOption(String compositeName) {
        return el(By.xpath(SPECIFIC_COMPOSITE_OPTION_XPATH_PREFIX + compositeName + "']"));
    }

    /**
     * Gets By associated with 'Copying simulation' alert
     *
     * @return by for alert window
     */
    private By settingUpAlert() {
        return By.xpath(CONTENT_AREA_XPATH_PREFIX + COPYING_SIM_ALERT_TEXT + "')]");
    }

    /**
     * Gets the value in the Composite Name text box
     *
     * @return the displayed Composite Name value
     */
    public String getCompositeName() {
        return getCompositeNameTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the Simulation Name text box
     *
     * @return the displayed Simulation Name value
     */
    public String getSimulationName() {
        return getSimulationNameTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the copy simulation header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isCopySimHeaderDisplayed() {
        return isHeaderDisplayed(COPY_SIM_HEADER_TEXT);
    }

    /**
     * Checks to see if the copy simulation help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isCopySimHelpHeaderDisplayed() {
        return isHeaderDisplayed(COPY_SIM_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Composite Name text box is displayed
     *
     * @return true if the text box is displayed, false otherwise
     */
    public boolean isCompositeNameTextBoxDisplayed() {
        return isElementDisplayed(getCompositeNameTextBox());
    }

    /**
     * Checks to see if the Simulation Name text box is displayed
     *
     * @return true if the text box is displayed, false otherwise
     */
    public boolean isSimulationNameTextBoxDisplayed() {
        return isElementDisplayed(getSimulationNameTextBox());
    }

    /**
     * Checks to see if the create button is displayed
     *
     * @return true if the create button is displayed, false otherwise
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getCreateBtn());
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false otherwise
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false otherwise
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the help header is displayed
     *
     * @return true if the help header is displayed, false otherwise
     */
    public boolean isHelpHeaderDisplayed() {
        return isHeaderDisplayed(COPY_SIM_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(COPY_SIM_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if an error appears when attempting to give the simulation
     * the same name as the composite
     *
     * @return true if the error appears, otherwise false
     */
    public boolean isSameNameErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(SIMULATION_COMPOSITE_SAME_NAME_ERROR));
    }

    /**
     * Checks to see if an error appears when using an invalid Simulation or
     * Composite name
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isBlankSimOrCompositeNameErrorDisplayed() {
        return isElementDisplayed(getFieldErrorRequiredXpath());
    }

    /**
     * Checks to see if an error appears when using invalid characters in the
     * Composite name field
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isInvalidCompositeNameErrorDisplayed() {
        return isInvalidNameErrorDisplayed(COMPOSITE_NAME_FIELD_LABEL);
    }

    /**
     * Checks to see if an error appears when using invalid characters in the
     * Sim name field
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isInvalidSimNameErrorDisplayed() {
        return isInvalidNameErrorDisplayed(SIMULATION_NAME_FIELD_LABEL);
    }

    /**
     * Checks to see if an error is present when leading or trailing spaces are
     * present in the Composite name field
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isCompositeLeadingTrailingWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(COMPOSITE_NAME_FIELD_LABEL);
    }

    /**
     * Checks to see if an error is present when leading or trailing spaces are
     * present in the Simulation name field
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isSimulationLeadingTrailingWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(SIMULATION_NAME_FIELD_LABEL);
    }

    /**
     * Checks to see if an error is present when two Simulations with the same
     * name are present in a Composite
     *
     * @param existingName -name of exisiting simulation
     * @return true if the error is displayed, otherwise false
     */
    public boolean isIdenticalSimInCompositeErrorDisplayed(String existingName) {
        return isElementDisplayed(getSpecificErrorToolTip(IDENTICAL_SIM_NAME_ERROR_PREFIX + existingName + IDENTICAL_SIME_NAME_ERROR_SUFFIX));
    }

    //TODO Generalize this method later. Currently using the generic isInvalidCharacterCompositeOrSimErrorDisplayed method errors out for Simulations only for an unknown reason.
    /**
     * Method to check if a Simulation error appears when an invalid name is
     * used
     *
     * @return true if the error is displayed, otherwise false
     */
    public boolean isSimNameRequiredErrorDisplayed() {
        return isElementDisplayed(By.xpath(TOOLTIP_ERROR_SIM_PREFIX + SIMULATION_NAME_REQUIRED_TEXT + "')]"));
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
     * Sets the name of the composite
     *
     * @param name -the name of the composite to set
     */
    public void setCompositeName(String name) {
        getCompositeNameTextBox().setText(name);
    }

    /**
     * Sets the name associated with the given composite
     *
     * @param sim -the composite being created
     */
    public void setCompositeName(CompositeSimulation sim) {
        String name = sim.getName();
        getCompositeNameTextBox().setText(name);
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
     * Sets the name of the simulation
     *
     * @param name -the name of the simulation to set
     */
    public void setSimulationName(String name) {
        getSimulationNameTextBox().setText(name);
    }

    /**
     * Sets the name associated with the given simulation
     *
     * @param sim -the template simulation being created
     */
    public void setSimulationName(TemplateSimulation sim) {
        String name = sim.getName();
        getSimulationNameTextBox().setText(name);
    }

    /**
     * Sets the name associated with the given simulation
     *
     * @param sim -the uploaded simulation being created
     */
    public void setSimulationName(UploadedSimulation sim) {
        String name = sim.getName();
        getSimulationNameTextBox().setText(name);
    }

    /**
     * Click the Create button
     *
     * @param success -true if success is expected, false otherwise
     * @return the newly loaded Simulations Page if success, otherwise null
     */
    public SimulationsPage clickCreate(boolean success) {
        getCreateBtn().click();
        if (success) {
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
     * @return the newly loaded Simulations Page
     */
    public SimulationsPage clickCancel() {
        getCancelBtn().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Click the Copy Sim Help icon
     */
    public void clickCopySimHelp() {
        getCopySimHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(COPY_SIM_HELP_HEADER_TEXT);
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + COPY_SIM_HELP_HEADER_TEXT + "']"));
    }

    /**
     * Clicks the Close icon
     *
     * @return the newly loaded Simulations Page
     */
    public SimulationsPage clickCloseIcon() {
        clickCloseIcon(COPY_SIM_HEADER_TEXT);
        return getPage(SimulationsPage.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkCopySimHeaderIcons() {
        checkHeaderIcons(COPY_SIM_HEADER_TEXT);
    }

    /**
     * Checks for expected help header and help content
     */
    public void checkHelpModal() {
        Assert.assertTrue("The Copy Simulation Help header is not displayed as expected.", isHelpHeaderDisplayed());
        Assert.assertTrue("The Copy Simulation help content is not displayed as expected", isHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Composite and Simulation Name text boxes
     */
    public void checkFields() {
        Assert.assertTrue("The Composite Name text box is not displayed.", isCompositeNameTextBoxDisplayed());
        Assert.assertTrue("The Simulation Name text box is not displayed.", isSimulationNameTextBoxDisplayed());
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button is not displayed.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks that displayed composite name matches the expected name
     *
     * @param name -the composite name expected
     */
    public void checkCompositeNameField(String name) {
        Assert.assertEquals("Displayed name does not match expected name.", name, getCompositeName());
    }

    /**
     * Checks that displayed composite name matches the name of the composite
     * given
     *
     * @param composite -the composite expected
     */
    public void checkCompositeNameField(CompositeSimulation composite) {
        String name = composite.getName();
        checkCompositeNameField(name);
    }

    /**
     * Checks that displayed simulation name matched expected name
     *
     * @param name -the simulation name expected
     */
    public void checkSimulationNameField(String name) {
        Assert.assertEquals("Displayed simulation name does not match expected name.", name, getSimulationName());
    }

    /**
     * Checks that displayed simulation name matches the name of the simulation
     * given
     *
     * @param sim -the template simulation expected
     */
    public void checkSimulationNameField(TemplateSimulation sim) {
        String name = sim.getName();
        checkSimulationNameField(name);
    }

    /**
     * Checks that displayed simulation name matches the name of the simulation
     * given
     *
     * @param sim -the uploaded simulation expected
     */
    public void checkSimulationNameField(UploadedSimulation sim) {
        String name = sim.getName();
        checkSimulationNameField(name);
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(CREATE_BTN_XPATH), 120);
    }

}
