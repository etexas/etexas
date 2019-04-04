package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation.Template;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the new simulation (from a template) form displayed
 * when New Template is selected
 *
 * @author llaroussini
 */
public class CreateSimulationFromTemplateModal extends CreateSimulationModal {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public CreateSimulationFromTemplateModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the create simulation from template header
     */
    private static final String CREATE_SIM_TEMPLATE_HEADER_TEXT = "Create Simulation from Template";

    /**
     * Text displayed in the create simulation from template help header
     */
    private static final String CREATE_SIM_TEMPLATE_HELP_HEADER_TEXT = "Create Simulation from Template Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Create a new simulation with the specified composite and name using the selected template. The simulation name must be unique in the composite. Entering the name of a composite that does not exist will create a new composite with the specified name to hold the created simulation.";

    /**
     * The xpath of the template dropdown
     */
    private static final String TEMPLATE_DROPDOWN_XPATH = "//input[@name='templateName']";

    /**
     * Xpath to template options displayed when template dropdown selected
     */
    private static final String TEMPLATE_OPTIONS_XPATH = "//li[@role='option'][contains(@data-boundview, 'simulation')][contains(@data-boundview, 'template')]";

    /**
     * Xpath prefix to specifc template option in options list
     */
    private static final String SPECIFIC_TEMPLATE_OPTION_XPATH_PREFIX = TEMPLATE_OPTIONS_XPATH + "[text()='";

    /**
     * The xpath of the template description area
     */
    private static final String TEMPLATE_DESCRIPTION_AREA_XPATH = "//span[text()='Description:']/ancestor::div[1]//div[contains(@id, 'inputEl')]";

    /**
     * Text displayed in Template Description area when no template is selected
     */
    private static final String NO_TEMPLATE_SELECTED_TEXT = "No template is selected";

    /**
     * Xpath prefix to all buttons
     */
    private static final String CREATE_TEMPLATE_SIM_FORM_BTN_XPATH_PREFIX = "//div[contains(@id, 'simulation')][contains(@id, 'Template')]//span[text()='";

    /**
     * Xpath to the Select button
     */
    private static final String SELECT_BTN_XPATH = CREATE_TEMPLATE_SIM_FORM_BTN_XPATH_PREFIX + "Select" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Create button
     */
    private static final String CREATE_BTN_XPATH = CREATE_TEMPLATE_SIM_FORM_BTN_XPATH_PREFIX + "Create" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Reset button
     */
    private static final String RESET_BTN_XPATH = CREATE_TEMPLATE_SIM_FORM_BTN_XPATH_PREFIX + "Reset" + BTN_XPATH_SUFFIX;

    /**
     * Xpath to the Cancel button
     */
    private static final String CANCEL_BTN_XPATH = CREATE_TEMPLATE_SIM_FORM_BTN_XPATH_PREFIX + "Cancel" + BTN_XPATH_SUFFIX;

    /**
     * Template field name as displayed in UI
     */
    private static final String TEMPLATE_FIELD_DISPLAYED_NAME = "Template";

    /**
     * The xpath of the template dropdown selector
     */
    private static final String TEMPLATE_DROPDOWN_SELECTOR_XPATH = "//div[contains(@id, 'simulation')][contains(@id, 'template')][contains(@class, 'x-form-arrow-trigger')]";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the template form content window to the help icon
     *
     * @return the help icon
     */
    private El getTemplateSimHelpIcon() {
        return getHelpIcon(CREATE_SIM_TEMPLATE_HEADER_TEXT);
    }

    /**
     * Gets the template dropdown menu
     *
     * @return the template dropdown
     */
    private El getTemplateDropdown() {
        return el(By.xpath(TEMPLATE_DROPDOWN_XPATH));
    }

    /**
     * Gets the template dropdown selector
     *
     * @return the template dropdown selector
     */
    private El getTemplateSelector() {
        return el(By.xpath(TEMPLATE_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the list of template options
     *
     * @return the list element
     */
    private List<El> getTemplateList() {
        return els(By.xpath(TEMPLATE_OPTIONS_XPATH));
    }

    /**
     * Gets specific option from dropdown list
     *
     * @param template -the template to get
     * @return the option element
     */
    private El getSpecificOption(String template) {
        return el(By.xpath(SPECIFIC_TEMPLATE_OPTION_XPATH_PREFIX + template + "']"));
    }

    /**
     * Gets the area displaying the selected template description
     *
     * @return the area element
     */
    private El getTemplateDescriptionArea() {
        return el(By.xpath(TEMPLATE_DESCRIPTION_AREA_XPATH));
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
     * Gets the value in the template dropdown
     *
     * @return the displayed Template value
     */
    public String getTemplate() {
        return getTemplateDropdown().getAttribute("value");
    }

    /**
     * Gets the value in the template description area
     *
     * @return the displayed Template Description value
     */
    public String getTemplateDescription() {
        return getTemplateDescriptionArea().getText();
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the create simulation from template header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateSimTemplateHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_SIM_TEMPLATE_HEADER_TEXT);
    }

    /**
     * Checks to see if the create simulation from template help header is
     * displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateSimTemplateHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_SIM_TEMPLATE_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isTemplateSimHelpIconDisplayed() {
        return isHelpIconDisplayed(CREATE_SIM_TEMPLATE_HEADER_TEXT);
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
     * Checks to see if the template dropdown is displayed
     *
     * @return true if the template dropdown is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isTemplateDropdownDisplayed() {
        return isElementDisplayed(getTemplateDropdown());
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
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks that the template description area is displayed
     *
     * @return true if the description area is displayed, false otherwise
     */
    public boolean isDescriptionDisplayed() {
        return isElementDisplayed(getTemplateDescriptionArea());
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_SIM_TEMPLATE_HELP_HEADER_TEXT);
    }

    /**
     * Checks if Template required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isTemplateRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(TEMPLATE_FIELD_DISPLAYED_NAME);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Selects the template of the given template simulation
     *
     * @param template -string value of the template to select
     */
    public void selectTemplate(String template) {
        selectFromDropdownList(getTemplateSelector(), getSpecificOption(template));
    }

    /**
     * Selects the template of the given template simulation
     *
     * @param sim -the template simulation
     */
    public void selectTemplate(TemplateSimulation sim) {
        String template = sim.getTemplate().getLabel();
        selectTemplate(template);
    }

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
     * Click the Create button
     *
     * @param success - true if success is expected, false otherwise
     * @return the newly loaded Simulation Page if success is true, if false no
     *         return
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
     * Click the Template Sim Help icon
     */
    public void clickTemplateSimHelp() {
        getTemplateSimHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_SIM_TEMPLATE_HELP_HEADER_TEXT);
    }

    /**
     * Clicks the Close icon associated with the Create Simulation from Template
     * modal
     *
     * @return the newly loaded SimulationsPage
     */
    public SimulationsPage clickCloseIcon() {
        clickCloseIcon(CREATE_SIM_TEMPLATE_HEADER_TEXT);
        return getPage(SimulationsPage.class);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for presence of Create Simulation from Template header
     */
    public void checkTemplateSimHeader() {
        Assert.assertTrue("Create Simulation from Template header is not displayed.", isHeaderDisplayed(CREATE_SIM_TEMPLATE_HEADER_TEXT));
    }

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkTemplateSimHeaderIcons() {
        checkHeaderIcons(CREATE_SIM_TEMPLATE_HEADER_TEXT);
    }

    /**
     * Verifies the Create Template Simulation Help modal is displayed (checks
     * header and content text)
     */
    public void checkHelpModal() {
        Assert.assertTrue("The Create Simulation from Template Help header is not displayed.", isCreateSimTemplateHelpHeaderDisplayed());
        Assert.assertTrue("The Create Simulation from Template Help content is not displayed.", isHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Composite Name text box, Select button,
     * Simulation Name text box, and Template dropdown
     */
    public void checkFields() {
        Assert.assertTrue("The Composite Name text box is not displayed.", isCompositeNameTextBoxDisplayed());
        Assert.assertTrue("The Select button is not displayed.", isSelectBtnDisplayed());
        Assert.assertTrue("The Simulation Name text box is not displayed.", isSimNameTextBoxDisplayed());
        Assert.assertTrue("The Template dropdown is not displayed.", isTemplateDropdownDisplayed());
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
     * Sets the name and template associated with the given template simulation
     *
     * @param sim -the template simulation being created
     */
    public void setAllFields(TemplateSimulation sim) {
        setCompositeName(sim);
        setSimName(sim);
        selectTemplate(sim);
        waitUntilLoaded();
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
     * Click the template dropdown and verifies the template options list is
     * displayed, then clicks the selector again to close the list
     */
    public void checkTemplateList() {
        getTemplateSelector().click();
        Assert.assertNotNull("The template list could not be found after clicking the template selector.", getTemplateList());
        getTemplateSelector().click();
    }

    /**
     * Verifies 'No template selected' text is displayed in Template Description
     * area
     */
    public void checkNoTemplateSelectedText() {
        Assert.assertEquals("Displayed template description does not indicate no template is selected as expected.", NO_TEMPLATE_SELECTED_TEXT, getTemplateDescription());
    }

    /**
     * Checks that all fields have been reset to default values
     *
     * @param compositeName - the default composite name expected
     */
    public void checkResetFields(String compositeName) {
        Assert.assertEquals("Displayed composite name not reset as expected.", compositeName, getCompositeName());
        Assert.assertEquals("Displayed simulation name not cleared as expected.", "", getSimName());
        Assert.assertEquals("Displayed template not cleared as expected.", "", getTemplate());
        checkNoTemplateSelectedText();
    }

    /**
     * Checks that all fields have been reset to default values
     *
     * @param composite - the composite expected
     */
    public void checkResetFields(CompositeSimulation composite) {
        String compositeName = composite.getName();
        checkResetFields(compositeName);
    }

    /**
     * Checks that displayed values match expected
     *
     * @param compositeName -the name of the composite expected
     * @param simName -the name of the simulation expected
     * @param template -the template expected
     * @param description -the description expected
     */
    public void checkDisplayedSetFields(String compositeName, String simName, String template, String description) {
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, getCompositeName());
        Assert.assertEquals("Displayed simulation name does not match expected name.", simName, getSimName());
        Assert.assertEquals("Displayed template does not match expected template.", template, getTemplate());
        Assert.assertEquals("Displayed template description does not match expected description.", description, getTemplateDescription());
    }

    /**
     * Checks that displayed values match given template simulation values
     *
     * @param sim -the template simulation expected
     */
    public void checkAllDisplayedFields(TemplateSimulation sim) {
        String compositeName = sim.getComposite().getName();
        String name = sim.getName();
        Template template = sim.getTemplate();
        String templateStr = template.getLabel();
        String templateDesc = template.getDescription();
        checkDisplayedSetFields(compositeName, name, templateStr, templateDesc);
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Template
     * dropdown
     */
    public void checkTemplateFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Template dropdown.", isFieldRequiredErrorDisplayed(TEMPLATE_FIELD_DISPLAYED_NAME));
    }

    ////////
    // Waits
    ///////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(TEMPLATE_DROPDOWN_SELECTOR_XPATH), 30);
    }

}
