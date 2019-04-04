package com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests;

import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CopyCompositeModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Copy a Composite test, TC-101/ITC-082
 *
 * @author llaroussini
 */
public class CopyACompositeTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Original simulation object used in test case
     */
    private TemplateSimulation simulation;

    /**
     * Composite object used in test case
     */
    private CompositeSimulation composite;

    /**
     * Original simulation object used in internal test case
     */
    private TemplateSimulation itcSimulation;

    /**
     * Composite object used in internal test case
     */
    private CompositeSimulation itcComposite;

    /**
     * Copied composite object used in test case
     */
    private CompositeSimulation copiedComposite;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user, simulation, and composite
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testUser, simulation, composite);

        //Get simulation and composite for ITC
        itcSimulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        itcComposite = itcSimulation.getComposite();
        ETexasEntityManager.addEntities(itcSimulation, itcComposite);

        //Copy existing composite
        copiedComposite = (CompositeSimulation)composite.clone();
        copiedComposite.setName(RandomStringGenerator.nextLetterString(15));

        //Login to eTexas, create simulations/composites and navigate to simulations page
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(simulation);
        ETexasSimulationUtils.createTemplateSimulation(itcSimulation);
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-101
     */
    @Test
    public void copyCompositeExternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Verify the following buttons are enabled: Create, Edit, and Delete.
        simPage.checkAllSimulationBtns();

        //Select a composite name from the Simulations list.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Verify the following options are displayed: Copy, Export, Rename, Composite Settings, and Reporting.
        simPage.checkEditOptions();

        //Click the Copy option.
        simPage.clickCopy();
        CopyCompositeModal copyModal = getPage(CopyCompositeModal.class);

        //Verify a Copy Composite modal is displayed.
        Assert.assertTrue("The Copy Composite modal is not displayed as expected.", copyModal.isCopyCompositeHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        copyModal.checkCopyCompositeHeaderIcons();

        //Click the ‘?’ icon.
        copyModal.clickCopyCompositeHelp();

        //Verify a Copy Composite Help modal is displayed with instructions for copying a composite.
        copyModal.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("Help OK button could not be found in Copy Composite Help modal", copyModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        copyModal.clickHelpOKBtn();

        //Verify that the Copy Composite Help modal closes.
        Assert.assertFalse("The Copy Composite Help modal is still displayed after clicking OK.", copyModal.isHelpHeaderDisplayed());

        //Verify that a Name text box is displayed in the Copy Composite modal.
        copyModal.checkFields();

        //Verify the following buttons are displayed at the bottom of the Copy Composite modal: Update, Reset, and Cancel.
        copyModal.checkBtns();

        //Enter a new name in the Name text box.
        copyModal.setName(copiedComposite);

        //Click the Reset button.
        copyModal.clickReset();

        //Verify the Name text box is reset to the original value.
        copyModal.checkNameField("");

        //Enter a new name in the Name text box.
        copyModal.setName(copiedComposite);

        //Click the Cancel button.
        copyModal.clickCancel();

        //Verify that the Copy Composite modal is no longer displayed.
        Assert.assertFalse("The Copy Composite modal is still displayed after clicking Cancel.", copyModal.isCopyCompositeHeaderDisplayed());

        //Verify that the new composite is not in the simulation list.
        Assert.assertFalse("Copied composite is displayed despite the Copy Composite modal being cancelled.", simPage.isCompositeDisplayed(copiedComposite));

        //Select an existing composite with no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Copy option.
        simPage.clickCopy();
        copyModal = getPage(CopyCompositeModal.class);

        //Verify that the Copy Composite modal is displayed.
        Assert.assertTrue("The Copy Composite modal is not displayed as expected.", copyModal.isCopyCompositeHeaderDisplayed());

        //Enter a new name in the Name text box.
        copyModal.setName(copiedComposite);

        //Click the ‘x’ icon.
        copyModal.clickCloseIcon();

        //Verify that the Copy Composite modal is no longer displayed.
        Assert.assertFalse("The Copy Composite modal is still displayed after clicking Close.", copyModal.isCopyCompositeHeaderDisplayed());

        //Verify that the new composite is not in the simulation list.
        Assert.assertFalse("Copied composite is displayed despite the Copy Composite modal being closed.", simPage.isCompositeDisplayed(copiedComposite));

        //Select an existing composite with no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Copy option.
        simPage.clickCopy();
        copyModal = getPage(CopyCompositeModal.class);

        //Verify that the Copy Composite modal is displayed.
        Assert.assertTrue("The Copy Composite modal is not displayed as expected.", copyModal.isCopyCompositeHeaderDisplayed());

        //Enter a new name in the Name text box.
        copyModal.setName(copiedComposite);

        //Click the Create button.
        copyModal.clickCreate(true);

        //Verify the Create Composite modal is no longer displayed.
        Assert.assertFalse("The Copy Composite modal is still displayed after clicking Create.", copyModal.isCopyCompositeHeaderDisplayed());

        //Verify the new composite is displayed in the list of simulations.
        Assert.assertTrue("Copied composite is not displayed as expected when succesfully created.", simPage.isCompositeDisplayed(copiedComposite));

        //Expand the new composite.
        simPage.expandComposite(copiedComposite, true);

        //Verify a copy of all simulations associated with the original composite are displayed with the newly copied composite.
        List<TemplateSimulation> templateSims = composite.getTemplateSims();
        for (TemplateSimulation templateSim : templateSims) {
            Assert.assertTrue("Simulation named: " + templateSim.getName() + " was not displayed with the copied composite as expected", simPage.isSimDisplayed(templateSim));
        }
        List<UploadedSimulation> uploadedSims = composite.getUploadedSims();
        for (UploadedSimulation uploadedSim : uploadedSims) {
            Assert.assertTrue("Simulation named: " + uploadedSim.getName() + " was not displayed with the copied composite as expected", simPage.isSimDisplayed(uploadedSim));
        }

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-082
     */
    @Test
    public void copyCompositeInternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a single existing composite with no executions.
        simPage.selectComposite(itcComposite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Copy option.
        simPage.clickCopy();
        CopyCompositeModal copyModal = getPage(CopyCompositeModal.class);

        //With the name field blank, click Create.
        copyModal.clickCreate(false);

        //Verify an error is displayed associated with the Name text box indicating a valid composite name is required.
        Assert.assertTrue("Name required error is not displayed as expected when Name text box is blank.", copyModal.isNameRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box.
        copyModal.setName("   ");

        //Verify an error is displayed associated with the Name text box indicating a valid composite name is required.
        Assert.assertTrue("Name required error is not displayed as expected when Name text box contains whitespace only.", copyModal.isNameRequiredErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        copyModal.setName(RandomStringGenerator.nextLetterString(5) + "$*&&#@)");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when special characters are used for composite name.", copyModal.isInvalidCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        copyModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when consecutive spaces are used in composite name.", copyModal.isInvalidCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        copyModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed with Composite Name text box when a leading space is used.",
                copyModal.isLeadingTrailingWhitespaceCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        copyModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed with Composite Name text box when a trailing space is used.",
                copyModal.isLeadingTrailingWhitespaceCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        copyModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when a leading dash ('-') is used in composite name.", copyModal.isInvalidCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        copyModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when a trailing dash ('-') is used in composite name.", copyModal.isInvalidCompositeNameErrorDisplayed());

        //Enter the name of an existing composite in the Name text box.
        copyModal.setName(composite);

        //Verify an error is displayed associated with the Name text box indicating the field requires a unique value.
        Assert.assertTrue("Duplicate Composite Name error not displayed as expected.", copyModal.isDuplicateCompositeNameErrorDisplayed(composite));

        //Enter a valid name in the Name text box.
        String newName = RandomStringGenerator.nextLetterString(10);
        copyModal.setName(newName);

        //Click the Create button.
        copyModal.clickCreate(true);

        //Verify the new name is displayed with the associated simulation in the simulations list.
        Assert.assertTrue("Copied composite is not displayed as expected when succesfully created.", simPage.isCompositeNameDisplayed(newName));

        //Logout
        simPage.logout(testUser);
    }
}
