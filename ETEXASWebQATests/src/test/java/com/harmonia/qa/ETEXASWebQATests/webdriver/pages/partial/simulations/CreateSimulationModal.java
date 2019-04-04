package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing elements/methods common to both the Create Simulation
 * from Template and Create Simulation from Upload modals
 *
 * @author llaroussini
 * @author rsmith
 */
public class CreateSimulationModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public CreateSimulationModal(WebDriver driver) {
        super(driver);
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * The xpath of the composite name text box
     */
    protected static final String COMPOSITE_NAME_TEXT_BOX_XPATH = "//input[@name='compositeName']";

    /**
     * The xpath of the composite options list
     */
    protected static final String COMPOSITE_OPTIONS_LIST_XPATH = "//div[contains(@id, 'menu')][@aria-expanded='true']";

    /**
     * Xpath prefix to specific composite option in options list
     */
    protected static final String SPECIFIC_COMPOSITE_OPTION_XPATH_PREFIX = COMPOSITE_OPTIONS_LIST_XPATH + "//span[text()='";

    /**
     * The xpath of the sim name text box
     */
    protected static final String SIM_NAME_TEXT_BOX_XPATH = "//input[@name='simulationName']";

    /**
     * Xpath suffix to all buttons
     */
    protected static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Text associated with setting up simulation alert
     */
    protected static final String SETTING_UP_SIM_ALERT_TEXT = "Building simulation";

    /**
     * Composite name field name as displayed in UI
     */
    protected static final String COMPOSITE_NAME_FIELD_DISPLAYED_NAME = "Composite";

    /**
     * Simulation name field name as displayed in UI
     */
    protected static final String SIM_NAME_FIELD_DISPLAYED_NAME = "Name";

    /**
     * Text displayed with error tool tip when invalid composite name is used
     */
    protected static final String INVALID_COMPOSITE_NAME_ERROR_TEXT = "Composite names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Text displayed with error tool tip when invalid simulation name is used
     */
    protected static final String INVALID_SIMULATION_NAME_ERROR_TEXT = "Simulation names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Text prefix displayed with error tool tip when duplicate simulation name
     * is used
     */
    protected static final String DUPLICATE_SIMULATION_NAME_ERROR_TEXT_PREFIX = "A simulation with the name \"";

    /**
     * Text suffix displayed with error tool tip when duplicate simulation name
     * is used
     */
    protected static final String DUPLICATE_SIMULATION_NAME_ERROR_TEXT_SUFFIX = "\" already exists in the composite.";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the composite name text box
     *
     * @return the composite name text box
     */
    protected El getCompositeNameTextBox() {
        return el(By.xpath(COMPOSITE_NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the list of composite options (displayed when select button is
     * clicked)
     *
     * @return the list of elements
     */
    protected List<El> getCompositeOptionsList() {
        return els(By.xpath(COMPOSITE_OPTIONS_LIST_XPATH));
    }

    /**
     * Gets the composite option with the given composite name
     *
     * @param compositeName -the name of the composite
     * @return the option element
     */
    protected El getSpecificCompositeOption(String compositeName) {
        return el(By.xpath(SPECIFIC_COMPOSITE_OPTION_XPATH_PREFIX + compositeName + "']"));
    }

    /**
     * Gets the simulation name text box
     *
     * @return the simulation name text box
     */
    protected El getSimNameTextBox() {
        return el(By.xpath(SIM_NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets By associated with 'Setting Up Simulation' alert
     *
     * @return by for alert window
     */
    protected By settingUpAlert() {
        return By.xpath(CONTENT_AREA_XPATH_PREFIX + SETTING_UP_SIM_ALERT_TEXT + "')]");
    }

    /**
     * Gets the value in the composite name text box
     *
     * @return the displayed composite name value
     */
    public String getCompositeName() {
        return getCompositeNameTextBox().getAttribute("value");
    }

    /**
     * Gets the value in the simulation name text box
     *
     * @return the displayed simulation name value
     */
    public String getSimName() {
        return getSimNameTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the composite name text box is displayed
     *
     * @return true if the composite name text box is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isCompositeNameTextBoxDisplayed() {
        return isElementDisplayed(getCompositeNameTextBox());
    }

    /**
     * Checks to see if the given composite name is displayed as an option
     *
     * @param compositeName -the name of the composite expected
     * @return true if the option is displayed, false otherwise
     */
    public boolean isSpecificCompositeOptionDisplayed(String compositeName) {
        return isElementDisplayed(getSpecificCompositeOption(compositeName));
    }

    /**
     * Checks to see if the simulation name text box is displayed
     *
     * @return true if the simulation name text box is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isSimNameTextBoxDisplayed() {
        return isElementDisplayed(getSimNameTextBox());
    }

    /**
     * Checks if Composite Name required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCompositeNameRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(COMPOSITE_NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks if Simulation Name required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSimulationNameRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(SIM_NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks if Invalid Composite Name error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCompositeNameInvalidErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_COMPOSITE_NAME_ERROR_TEXT);
    }

    /**
     * Checks if Invalid Simulation Name error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSimulationNameInvalidErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_SIMULATION_NAME_ERROR_TEXT);
    }

    /**
     * Checks if Leading/Trailing whitespace error is displayed with Composite
     * Name field
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCompositeNameLeadingTrailingSpacesErrorDisplayed() {
        return isWhitespaceErrorDisplayed(COMPOSITE_NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks if Leading/Trailing whitespace error is displayed with Simulation
     * Name field
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSimulationNameLeadingTrailingSpacesErrorDisplayed() {
        return isWhitespaceErrorDisplayed(SIM_NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks if Duplicate Simulation Name error is displayed
     *
     * @param simName -the duplicate simulation name
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateSimulationNameErrorDisplayed(String simName) {
        return isErrorToolTipDisplayed(DUPLICATE_SIMULATION_NAME_ERROR_TEXT_PREFIX + simName + DUPLICATE_SIMULATION_NAME_ERROR_TEXT_SUFFIX);
    }

    /**
     * Checks if Duplicate Simulation Name error is displayed
     *
     * @param sim -the duplicate simulation
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateSimulationNameErrorDisplayed(TemplateSimulation sim) {
        String simName = sim.getName();
        return isDuplicateSimulationNameErrorDisplayed(simName);
    }

    /**
     * Checks if Duplicate Simulation Name error is displayed
     *
     * @param sim -the duplicate simulation
     * @return true if displayed, false otherwise
     */
    public boolean isDuplicateSimulationNameErrorDisplayed(UploadedSimulation sim) {
        String simName = sim.getName();
        return isDuplicateSimulationNameErrorDisplayed(simName);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets the name of the composite associated with the simulation
     *
     * @param name -the name of the composite to set
     */
    public void setCompositeName(String name) {
        getCompositeNameTextBox().setText(name);
    }

    /**
     * Sets the composite name associated with the given simulation
     *
     * @param sim -the template simulation being created
     */
    public void setCompositeName(TemplateSimulation sim) {
        CompositeSimulation composite = sim.getComposite();
        String name = composite.getName();
        getCompositeNameTextBox().setText(name);
    }

    /**
     * Sets the composite name associated with the given simulation
     *
     * @param sim -the uploaded simulation being created
     */
    public void setCompositeName(UploadedSimulation sim) {
        CompositeSimulation composite = sim.getComposite();
        String name = composite.getName();
        getCompositeNameTextBox().setText(name);
    }

    /**
     * Sets the composite name associated with given composite
     *
     * @param composite -the composite being created
     */
    public void setCompositeName(CompositeSimulation composite) {
        String name = composite.getName();
        getCompositeNameTextBox().setText(name);
    }

    /**
     * Sets the name of the simulation
     *
     * @param name -the name of the simulation to set
     */
    public void setSimName(String name) {
        getSimNameTextBox().setText(name);
    }

    /**
     * Sets the name associated with the given simulation
     *
     * @param sim -the template simulation being created
     */
    public void setSimName(TemplateSimulation sim) {
        String name = sim.getName();
        getSimNameTextBox().setText(name);
    }

    /**
     * Sets the name associated with the given simulation
     *
     * @param sim -the uploaded simulation being created
     */
    public void setSimName(UploadedSimulation sim) {
        String name = sim.getName();
        getSimNameTextBox().setText(name);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Verifies the given composite name is displayed in the composite list
     * (visible only when select button has been clicked)
     *
     * @param compositeName -the name of the composite expected
     */
    public void checkSpecificCompositeInList(String compositeName) {
        Assert.assertTrue("Composite with name, " + compositeName + ", could not be found in the composite list.", isSpecificCompositeOptionDisplayed(compositeName));
    }

    /**
     * Verifies the given composite is displayed in the composite list (visible
     * only when select button has been clicked)
     *
     * @param composite -the composite expected
     */
    public void checkSpecificCompositeInList(CompositeSimulation composite) {
        String compositeName = composite.getName();
        checkSpecificCompositeInList(compositeName);
    }

    /**
     * Checks that displayed composite name matches expected composite name
     *
     * @param compositeName -the composite name expected
     */
    public void checkCompositeNameField(String compositeName) {
        Assert.assertEquals("Displayed composite name does not match expected simulation name.", compositeName, getCompositeName());
    }

    /**
     * Checks that displayed simulation name matches expected simulation name
     *
     * @param simName -the simulation name expected
     */
    public void checkSimNameField(String simName) {
        Assert.assertEquals("Displayed simulation name does not match expected simulation name.", simName, getSimName());
    }

    /**
     * Checks that displayed composite name and simulation name match the
     * expected values
     *
     * @param compositeName -composite name expected
     * @param simName -simulation name expected
     */
    public void checkDisplayedValues(String compositeName, String simName) {
        checkCompositeNameField(compositeName);
        checkSimNameField(simName);
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Composite Name
     * text box
     */
    public void checkCompositeNameFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Composite Name text box.", isFieldRequiredErrorDisplayed(COMPOSITE_NAME_FIELD_DISPLAYED_NAME));
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Simulation Name
     * text box
     */
    public void checkSimNameFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Simulation Name text box.", isFieldRequiredErrorDisplayed(SIM_NAME_FIELD_DISPLAYED_NAME));
    }

    /**
     * Verifies leading/trailing whitespace error icon/tooltip displayed with
     * Composite Name text box
     */
    public void checkCompositeNameWhitespaceErrorDisplayed() {
        Assert.assertTrue("Leading/trailing whitespace error not displayed as expected with Name text box.", isWhitespaceErrorDisplayed(COMPOSITE_NAME_FIELD_DISPLAYED_NAME));
    }

    /**
     * Verifies leading/trailing whitespace error icon/tooltip displayed with
     * Simulation Name text box
     */
    public void checkSimNameWhitespaceErrorDisplayed() {
        Assert.assertTrue("Leading/trailing whitespace error not displayed as expected with Name text box.", isWhitespaceErrorDisplayed(SIM_NAME_FIELD_DISPLAYED_NAME));
    }
}
