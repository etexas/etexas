package com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CopySimulationModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Copy a Simulation test, TC-005 and
 * ITC-004
 *
 * @author llaroussini
 * @author saistrop
 */
public class CopyASimulationTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Original Simulation object used in test case
     */
    private TemplateSimulation simulation;

    /**
     * Original alternate Simulation object used in test case
     */
    private TemplateSimulation altSimulation;

    /**
     * ITC Simulation object used in test case
     */
    private TemplateSimulation itcSimulation1;

    /**
     * Second ITC Simulation object used in test case
     */
    private TemplateSimulation itcSimulation2;

    /**
     * ITC composte object used in test case
     */
    private CompositeSimulation itcComposite;

    /**
     * Composite object used in test case
     */
    private CompositeSimulation composite;

    /**
     * Secondary composite used in test case
     */
    private CompositeSimulation altComposite;

    /**
     * New composite used in test case
     */
    private CompositeSimulation newComposite;

    /**
     * Copied Simulation object used in test case (first)
     */
    private TemplateSimulation copiedSim1;

    /**
     * Copied Simulation object used in test case (second)
     */
    private TemplateSimulation copiedSim2;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template Simulation
        composite = simulation.getComposite();
        altSimulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template Simulation
        altComposite = altSimulation.getComposite();
        itcSimulation1 = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template Simulation
        itcComposite = itcSimulation1.getComposite();
        itcSimulation2 = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template Simulation
        itcSimulation2.setComposite(itcComposite);
        ETexasEntityManager.addEntities(testUser, simulation, composite, altSimulation, altComposite, itcSimulation1, itcSimulation2, itcComposite);

        //Create a new composite object to be used in test case
        newComposite = new CompositeSimulation();
        newComposite.setName(RandomStringGenerator.nextLetterString(15));

        //Copy existing Simulation twice
        copiedSim1 = (TemplateSimulation)simulation.clone();
        copiedSim1.setName(RandomStringGenerator.nextLetterString(15));
        copiedSim2 = (TemplateSimulation)simulation.clone();
        copiedSim2.setName(RandomStringGenerator.nextLetterString(15));

        //Login to eTexas, create Simulations/composites and navigate to Simulations page
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(simulation);
        ETexasSimulationUtils.createTemplateSimulation(altSimulation);
        ETexasSimulationUtils.createTemplateSimulation(itcSimulation1);
        ETexasSimulationUtils.createTemplateSimulation(itcSimulation2);
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-005
     */
    @Test
    public void copySimExternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Verify the following buttons are enabled: Create, Edit, and Delete.
        simPage.checkAllSimulationBtns();

        //Select a Simulation name from the Simulations list.
        simPage.expandComposite(composite, true);
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Verify the following options are displayed: Copy, Export, Rename, Simulation Settings, and Simulation Source.
        simPage.checkEditOptions();

        //Click the Copy option.
        simPage.clickCopy();
        CopySimulationModal copyModal = getPage(CopySimulationModal.class);

        //Verify a Copy Simulation modal is displayed.
        Assert.assertTrue("The Copy Simulation modal is not displayed after initially clicking 'Copy'.", copyModal.isCopySimHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        copyModal.checkCopySimHeaderIcons();

        //Click the ‘?’ icon.
        copyModal.clickCopySimHelp();

        //Verify a Copy Simulation Help modal is displayed with instructions for copying a Simulation.
        copyModal.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("OK button not displayed in Copy Simulation Help modal.", copyModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        copyModal.clickHelpOKBtn();

        //Verify that the Copy Simulation Help modal closes.
        Assert.assertFalse("The Copy Simulation Help modal is still displayed after clicking 'OK'.", copyModal.isHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Copy Simulation modal: Composite and Name.
        copyModal.checkFields();

        //Verify the following buttons are displayed at the bottom of the Copy Simulation modal: Update, Reset, and Cancel.
        copyModal.checkBtns();

        //Enter or select a name for the composite in the Composite text box.
        copyModal.selectComposite(altComposite);

        //Enter a name for the Simulation into the Name text box.
        String copiedSim1Name = copiedSim1.getName();
        copyModal.setSimulationName(copiedSim1Name);

        //Click the Reset button.
        copyModal.clickReset();

        //Verify the Composite text box is reset to the original value.
        copyModal.checkCompositeNameField(composite);

        //Verify the Name text box is reset to the original value.
        copyModal.checkSimulationNameField("");

        //Enter or select a name for the composite in the Composite text box.
        copyModal.selectComposite(altComposite);

        //Enter a new name in the Name text box.
        copyModal.setSimulationName(copiedSim1Name);

        //Click the Cancel button.
        copyModal.clickCancel();

        //Verify that the Copy Simulation modal is no longer displayed.
        Assert.assertFalse("The Copy Simulation modal is still displayed after clicking Cancel.", copyModal.isCopySimHeaderDisplayed());

        //Verify that the new Simulation is not in the Simulation list.
        Assert.assertFalse("Copied Simulation is displayed despite cancelling in the Copy Simulation modal.", simPage.isSimDisplayed(copiedSim1));

        //Select an existing Simulation with no executions.
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Copy option.
        simPage.clickCopy();
        copyModal = getPage(CopySimulationModal.class);

        //Verify that the Copy Simulation modal is displayed.
        Assert.assertTrue("The Copy Simulation modal is not displayed as exepcted.", copyModal.isCopySimHeaderDisplayed());

        //Enter or select a name for the composite in the Composite text box.
        copyModal.selectComposite(altComposite);

        //Enter a new name in the Name text box.
        copyModal.setSimulationName(copiedSim1Name);

        //Click the ‘x’ icon.
        copyModal.clickCloseIcon();

        //Verify that the Copy Simulation modal is no longer displayed.
        Assert.assertFalse("The Copy Simulation modal is still displayed after clicking Close.", copyModal.isCopySimHeaderDisplayed());

        //Verify that the new Simulation is not in the Simulation list.
        Assert.assertFalse("Copied Simulation is displayed despite closing the Copy Simulation modal.", simPage.isSimDisplayed(copiedSim1));

        //Select an existing Simulation with no executions.
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Copy option.
        simPage.clickCopy();
        copyModal = getPage(CopySimulationModal.class);

        //Verify that the Copy Simulation modal is displayed.
        Assert.assertTrue("The Copy Simulation modal is not displayed as exepcted.", copyModal.isCopySimHeaderDisplayed());

        //Click the Select button next to the Composite text box and select any existing composite.
        copyModal.selectComposite(altComposite);

        //Enter a new name in the Name text box.
        copyModal.setSimulationName(copiedSim1Name);

        //Click the Create button.
        copyModal.clickCreate(true);

        //Verify the Create Simulation modal is no longer displayed.
        Assert.assertFalse("The Copy Simulation modal is still displayed after clicking Create.", copyModal.isCopySimHeaderDisplayed());

        //Verify the new Simulation is displayed under the Composite selected previously.
        simPage.expandComposite(altComposite, false);
        simPage.expandComposite(altComposite, true);
        Assert.assertTrue("Copied Simulation is not displayed despite successfully creating a Simulation copy.", simPage.isSimDisplayed(copiedSim1));

        //Verify the Type and Source columns values match the original Simulation from which the copy was made.
        Assert.assertEquals("The value in the Type column for the newly copied Simulation is not displayed as expected.", simulation.getSimType().getLabel(), simPage.getSimType(copiedSim1));
        Assert.assertEquals("The value in the Source column for the newly copied Simulation is not displayed as expected.", "Template: " + simulation.getTemplate().getLabel(),
                simPage.getSimSource(copiedSim1));

        //Select an existing Simulation with no executions.
        simPage.expandComposite(composite, true);
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Copy option.
        simPage.clickCopy();
        copyModal = getPage(CopySimulationModal.class);

        //Verify that the Copy Simulation modal is displayed.
        Assert.assertTrue("The Copy Simulation modal is not displayed as exepcted.", copyModal.isCopySimHeaderDisplayed());

        //Enter a new name in the Composite text box.
        String newCompositeName = newComposite.getName();
        copyModal.setCompositeName(newCompositeName);

        //Enter a new name in the Name text box.
        String newSimName = copiedSim2.getName();
        copyModal.setSimulationName(newSimName);

        //Click the Create button.
        copyModal.clickCreate(true);

        //Verify the Create Simulation modal is no longer displayed.
        Assert.assertFalse("The Copy Simulation modal is still displayed after clicking Create.", copyModal.isCopySimHeaderDisplayed());

        //Verify a new composite is displayed in the list of Simulations.
        Assert.assertTrue("The new composite is not displayed in the Simulation list after successfully creating a new Simulation copy in a new composite.",
                simPage.isCompositeDisplayed(newComposite));

        //Verify the new Simulation is displayed under the new composite.
        simPage.expandComposite(composite, false);
        simPage.expandComposite(altComposite, false);
        simPage.expandComposite(newComposite, true);
        Assert.assertTrue("Copied Simulation is not displayed despite successfully creating a Simulation copy in a new composite.", simPage.isSimDisplayed(newSimName));

        //Verify the Type and Source columns values match the original Simulation from which the copy was made.
        Assert.assertEquals("The value in the Type column for the newly copied Simulation is not displayed as expected.", simulation.getSimType().getLabel(), simPage.getSimType(copiedSim2));
        Assert.assertEquals("The value in the Source column for the newly copied Simulation is not displayed as expected.", "Template: " + simulation.getTemplate().getLabel(),
                simPage.getSimSource(copiedSim2));

        //Log out
        simPage.logout(testUser);

    }

    /**
     * Test steps for ITC-004
     */
    @Test
    public void copySimInternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
        //Verify Simulation page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select existing Simulation
        simPage.selectComposite(itcComposite, true);
        simPage.expandComposite(itcComposite, true);
        simPage.selectSim(itcSimulation1, true);

        //Click the Copy button
        simPage.clickEdit();
        simPage.clickCopy();
        CopySimulationModal copyForm = getPage(CopySimulationModal.class);

        //With Composite name field blank, click Create
        copyForm.setCompositeName("");
        copyForm.clickCreate(false);

        //Verify field required error is displayed associated with Name text box
        copyForm.isBlankSimOrCompositeNameErrorDisplayed();

        //Set Simulation name to the same as the Composite name
        copyForm.setSimulationName(itcComposite.getName());
        copyForm.setCompositeName(itcComposite);
        copyForm.clickCreate(false);

        //Verify error appears stating that composite and Simulation can't have the same name
        Assert.assertTrue("No error was reported when attempting to copy Simulation using the same name as the Composite.", copyForm.isSameNameErrorDisplayed());

        //Enter whitespace only in Name text box, click Create
        copyForm.setCompositeName("    ");
        copyForm.setSimulationName("Test");
        copyForm.clickCreate(false);

        //Verify error appears stating that the Composite must have a valid name
        Assert.assertTrue("Error did not appear when a blank composite name was set.", copyForm.isBlankSimOrCompositeNameErrorDisplayed());

        //Enter invalid name (contains special characters), click Create
        copyForm.setCompositeName("Te$t!");
        copyForm.clickCreate(false);

        //Verify error message displayed associated with Name text box
        Assert.assertTrue("Invalid Composite Name error tooltip could not be found after entering a Composite name with special characters.",
                copyForm.isInvalidCompositeNameErrorDisplayed());

        //Enter invalid name (contains consecutive spaces), click Create
        copyForm.setCompositeName("Te  st");
        copyForm.clickCreate(false);

        //Verify error message displayed associated with Name text box
        Assert.assertTrue("Invalid Composite Name error tooltip could not be found after entering a Composite name with consecutive spaces.",
                copyForm.isInvalidCompositeNameErrorDisplayed());

        //Enter invalid name (contains leading space), click Create
        copyForm.setCompositeName(" Test");
        copyForm.clickCreate(false);

        //Verify leading/trailing space error message displayed associated with Name text box
        Assert.assertTrue("An error regarding leading and trailing spaces did not occur when a Composite name contained a leading space.",
                copyForm.isCompositeLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value for the Composite name that contains a trailing space
        copyForm.setCompositeName("Test ");
        copyForm.clickCreate(false);

        //Verify error message displayed associated with Name text box
        Assert.assertTrue("An error regarding leading and trailing spaces did not occur when a Composite name contained a trailing space.",
                copyForm.isCompositeLeadingTrailingWhitespaceErrorDisplayed());

        //Enter a value in the Composite name that contains a leading hyphen
        copyForm.setCompositeName("-Test");
        copyForm.clickCreate(false);

        //Verify an error appears indicating acceptable values for the field
        Assert.assertTrue("Invalid Composite Name error tooltip could not be found after entering a Composite name with a leading hyphen.", copyForm.isInvalidCompositeNameErrorDisplayed());

        //Enter a value in the Composite name that contains a trailing hyphen
        copyForm.setCompositeName("Test-");
        copyForm.clickCreate(false);

        //Verify an error appears indicating acceptable values for the field
        Assert.assertTrue("Invalid Composite Name error tooltip could not be found after entering a Composite name with a trailing hyphen.", copyForm.isInvalidCompositeNameErrorDisplayed());

        //Reset Composite name and enter spaces into the Name field
        copyForm.setCompositeName(itcComposite);
        copyForm.setSimulationName("    ");

        //Verify error appears stating a valid Simulation name is required
        Assert.assertTrue("No error was displayed when an invalid Simulation name was used when one was expected.", copyForm.isSimNameRequiredErrorDisplayed());

        //Set an invalid Simulation name that doesn't contain hyphens and verify error appears
        copyForm.setSimulationName("Te$t");
        Assert.assertTrue("An error was not displayed when using invalid characters in the Simulation name.", copyForm.isInvalidSimNameErrorDisplayed());

        //Set a Simulation name with consecutive spaces in the middle and verify an error appears
        copyForm.setSimulationName("Te  st");
        Assert.assertTrue("An error was not displayed when using consecutive spaces in the test name when one was expected.", copyForm.isInvalidSimNameErrorDisplayed());

        //Set a Simulation name with consecutive spaces at the beginning and verify an error appears
        copyForm.setSimulationName("  Test");
        Assert.assertTrue("An error was not displayed when using consecutive spaces before the test name when one was expected.", copyForm.isSimulationLeadingTrailingWhitespaceErrorDisplayed());

        //Set a Simulation name with consecutive spaces at the end and verify an error appears
        copyForm.setSimulationName("Test  ");
        Assert.assertTrue("An error was not displayed when using consecutive spaces after the test name when one was expected.", copyForm.isSimulationLeadingTrailingWhitespaceErrorDisplayed());

        //Set a Simulation name with a leading hyphen and verify an error appears
        copyForm.setSimulationName("-Test");
        Assert.assertTrue("An error was not displayed when using a leading hyphen in the test name when one was expected.", copyForm.isInvalidSimNameErrorDisplayed());

        //Set a Simulation name with a trailing hyphen and verify an error appears
        copyForm.setSimulationName("Test-");
        Assert.assertTrue("An error was not displayed when using a trailing hyphen in the test name when one was expected.", copyForm.isInvalidSimNameErrorDisplayed());

        //Set a Simulation name that is the same as another Simulation within the same Composite and verify an error appears
        copyForm.setSimulationName(itcSimulation2.getName());
        Assert.assertTrue("An error was not displayed when using a test name identical to another Simulation when one was expected.",
                copyForm.isIdenticalSimInCompositeErrorDisplayed(itcSimulation2.getName()));

        //Enter a valid new name for the Simulation and verify the Simulation appears in the list
        itcSimulation1.setName(RandomStringGenerator.nextLetterString(15));
        copyForm.setSimulationName(itcSimulation1.getName());
        simPage = copyForm.clickCreate(true);
        Assert.assertTrue("The new Simulation name is not listed in the current list of Simulations for the selected Composite.", simPage.isSimDisplayed(itcSimulation1.getName()));

        //Update entity manager
        TemplateSimulation sim = ETexasEntityManager.getTemplateSimulation(itcSimulation1.getName());
        sim.setName(itcSimulation1.getName());
        sim.setComposite(itcComposite);
        sim.setUser(testUser);

        //Log out
        simPage.logout(testUser);
    }
}
