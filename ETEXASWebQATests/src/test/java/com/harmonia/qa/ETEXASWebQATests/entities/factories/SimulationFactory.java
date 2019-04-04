package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import java.util.ArrayList;
import java.util.List;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationFoundation;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationType;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation.Template;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating simulation objects
 *
 * @author llaroussini
 */
public class SimulationFactory {

    /**
     * Returns a new instance of the template simulation object
     *
     * @return a new simulation object
     */
    public static TemplateSimulation getTemplateSimulation() {
        return instantiateTemplateSimulation();
    }

    /**
     * Returns a new instance of the uploaded simulation object (note that this
     * object has NOT actually been created in the application)
     *
     * @return a new uploaded simulation object
     */
    public static UploadedSimulation getUploadedSimulation() {
        UploadedSimulation sim = instantiateUploadedSimulation();
        sim.setSimFoundation(SimulationFoundation.UPLOAD);
        return sim;
    }

    /**
     * Gets a new template simulation which may be random or static
     *
     * @param user -the user associated with the simulation
     * @param random whether or not the returned simulation should have static
     *        or randomized values assigned
     * @return the newly created simulation according to the randomization
     *         parameter
     */
    public static TemplateSimulation getTemplateSimulation(ETexasUser user, boolean random) { //TODO add uploaded simulations
        if (random) {
            return getRandomTemplateSimulation(user);
        }
        else {
            return getStaticTemplateSimulation(user);
        }
    }

    /**
     * Gets an optionally randomized uploaded simulation
     *
     * @param user -the user associated with the simulation
     * @param random whether the desired simulation is random or not
     * @return the newly created uploaded simulaiton
     */
    public static UploadedSimulation getUploadedSimulation(ETexasUser user, boolean random) {
        if (random) {
            return getRandomUploadedSimulation(user);
        }
        else {
            return getStaticUploadedSimulation(user);
        }
    }

    /**
     * Gets a template simulation with randomly assigned values
     *
     * @param user -the user associated with the simulation
     * @return a new template simulation with random values
     */
    private static TemplateSimulation getRandomTemplateSimulation(ETexasUser user) { //TODO randomize to include uploaded simulations, or create a getRandomUploadedSimulation
        List<TemplateSimulation> templateSims = new ArrayList<TemplateSimulation>();
        TemplateSimulation sim = getTemplateSimulation();
        sim.setSimFoundation(SimulationFoundation.TEMPLATE);
        sim.setSimType(SimulationType.ETEXAS);
        sim.setName(RandomStringGenerator.nextLetterString(15));
        Template template = Template.values()[RandomNumberGenerator.nextInteger(Template.values().length)];
        sim.setTemplate(template);
        sim.setUser(user);
        CompositeSimulation composite = new CompositeSimulation();
        composite.setName(RandomStringGenerator.nextLetterString(10));
        composite.setTemplateSims(templateSims);
        sim.setComposite(composite);
        List<Simulation> sims = new ArrayList<Simulation>(1);
        sims.add(sim);
        user.setSimulations(sims);
        return sim;
    }

    /**
     * Gets a random uploaded simulation object
     *
     * @param user -the user associated with the simulation
     * @return a newly created randomized uploaded simulation
     */
    private static UploadedSimulation getRandomUploadedSimulation(ETexasUser user) {
        List<UploadedSimulation> uploadedSims = new ArrayList<UploadedSimulation>();
        UploadedSimulation sim = getUploadedSimulation();
        sim.setSimType(RandomNumberGenerator.flipCoin() ? SimulationType.ETEXAS : SimulationType.PLAYBACK);
        sim.setName(RandomStringGenerator.nextLetterString(15));
        sim.setUser(user);
        CompositeSimulation composite = new CompositeSimulation();
        composite.setName(RandomStringGenerator.nextLetterString(10));
        composite.setUploadedSims(uploadedSims);
        sim.setComposite(composite);
        List<Simulation> sims = new ArrayList<Simulation>(1);
        sims.add(sim);
        user.setSimulations(sims);
        return sim;
    }

    /**
     * Gets a template simulation with a known set of values
     *
     * @param user -the user associated with the simulation
     * @return a new template simulation with known values
     */
    private static TemplateSimulation getStaticTemplateSimulation(ETexasUser user) {//TODO randomize to include uploaded simulations, or create a getStaticUploadedSimulation
        List<TemplateSimulation> templateSims = new ArrayList<TemplateSimulation>();
        TemplateSimulation sim = getTemplateSimulation();
        sim.setSimFoundation(SimulationFoundation.TEMPLATE);
        sim.setSimType(SimulationType.ETEXAS);
        sim.setName(RandomStringGenerator.nextLetterString(5));
        sim.setTemplate(Template.EX_05);
        sim.setUser(user);
        CompositeSimulation composite = new CompositeSimulation();
        composite.setName(RandomStringGenerator.nextLetterString(10));
        sim.setComposite(composite);
        composite.setTemplateSims(templateSims);
        List<Simulation> sims = new ArrayList<Simulation>(1);
        sims.add(sim);
        user.setSimulations(sims);
        return sim;
    }

    /**
     * Gets an uploaded simulation object with static, predictable values
     *
     * @param user -the user associated with the simulation
     * @return a static uploaded simulation object
     */
    private static UploadedSimulation getStaticUploadedSimulation(ETexasUser user) {
        List<UploadedSimulation> uploadedSims = new ArrayList<UploadedSimulation>();
        UploadedSimulation sim = getUploadedSimulation();
        sim.setSimType(SimulationType.ETEXAS);
        sim.setName("Static Uploaded Simulation");
        sim.setUser(user);
        CompositeSimulation composite = new CompositeSimulation();
        composite.setName(RandomStringGenerator.nextLetterString(10));
        composite.setUploadedSims(uploadedSims);
        sim.setComposite(composite);
        List<Simulation> sims = new ArrayList<Simulation>(1);
        sims.add(sim);
        user.setSimulations(sims);
        return sim;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed template simulation object
     */
    private static TemplateSimulation instantiateTemplateSimulation() {
        return new TemplateSimulation();
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed uploaded simulation object
     */
    private static UploadedSimulation instantiateUploadedSimulation() {
        return new UploadedSimulation();
    }

}
