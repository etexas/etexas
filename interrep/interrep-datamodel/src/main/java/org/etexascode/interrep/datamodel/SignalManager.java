/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.interrep.datamodel;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.interrep.datamodel.xmladapters.JaxbMapOfListsAdapter;

/**
 * The signal manager model.
 * 
 * @author jrutherford
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "SignalManger")
@XmlSeeAlso(value = SignalIndication.class)
public class SignalManager implements Serializable, ISignalManager {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 2786568932989827369L;

    /** The list of signals. */
    @XmlElement
    @XmlJavaTypeAdapter(JaxbMapOfListsAdapter.class)
    private Map<Integer, List<SignalIndication>> signals = new HashMap<Integer, List<SignalIndication>>();

    /**
     * Adds a SignalIndication to the list.
     * 
     * @param signal The new SignalIndication.
     */
    public void addSignal(SignalIndication signal) {
        List<SignalIndication> signalList = signals.get(signal.getLaneId());
        if (signalList == null) {
            signalList = new LinkedList<SignalIndication>();
            signals.put(signal.getLaneId(), signalList);
        }
        signalList.add(signal);
    }

    /** String representation. */
    @CoberturaIgnore
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        UtilsStringOnModel.addMap(ret, signals, "signals");

        return ret.toString();
    }

    /** Check if 2 signal managers are equal. */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SignalManager) {
            SignalManager sm = (SignalManager)obj;
            return signals.equals(sm.signals);
        }
        else {
            return false;
        }

    }

    /**
     * HashCode operation. TODO: ablatt - this hash code is clearly wrong...
     * 
     * @return A hashcode of the data.
     */
    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(97, 17).toHashCode();
    }

    /**
     * Get a list of SignalIndications by laneId.
     * 
     * @param laneId The laneId to get indications for.
     * @return The list of indications that apply to the given lane. Null if there are no
     *         indications for that lane id.
     */
    @Override
    public List<SignalIndication> getSignalsByLaneId(int laneId) {
        return signals.get(laneId);
    }

    /**
     * Adds a list of signals.
     * 
     * @param signals The list of signals to add.
     */
    public void addSignals(List<SignalIndication> signals) {
        for (SignalIndication si : signals) {
            addSignal(si);
        }
    }

    /**
     * Gets the keys for the signals map. This is the lanes which this signal manager contains
     * signals for.
     * 
     * @return The keys in signal
     */
    @CoberturaIgnore
    @Override
    public Set<Integer> keysForSigMap() {
        return signals.keySet();
    }

    /**
     * Special iterator helper for signal manager to parse through signal indications
     * 
     * @return The iterator helper
     */
    @CoberturaIgnore
    @Override
    public Iterator<ISignalIndication> iterator() {
        return new IteratorHelper<ISignalIndication, SignalIndication>(new SigManIterator(signals.entrySet()));
    }

    /**
     * Gets the iterable signal indication
     * 
     * @return The entry set
     */
    public Iterable<SignalIndication> getIterable() {
        return new IterableHelper<SignalIndication>(new SigManIterator(signals.entrySet()));
    }

    /**
     * Specialized iterator across a map of lists.
     * 
     * @author ablatt
     */
    private static class SigManIterator implements Iterator<SignalIndication> {

        /**
         * Top level iterator. Contains a sequence of lists we need to iterate over
         */
        private Iterator<Entry<Integer, List<SignalIndication>>> entSetIter;

        /**
         * The current iterator we are using
         */
        private Iterator<SignalIndication> curr = null;

        /**
         * Constructor
         * 
         * @param entSet Top level iterator
         */
        private SigManIterator(Set<Entry<Integer, List<SignalIndication>>> entSet) {
            entSetIter = entSet.iterator();
        }

        /**
         * Checks if the iterator has a next value
         * 
         * @return True/False
         */
        @Override
        public boolean hasNext() {
            return entSetIter.hasNext() || (curr != null);
        }

        /**
         * Gets the next value in the iterator
         * 
         * @return ret the next value
         */
        @Override
        public SignalIndication next() {
            if (curr == null) {
                curr = entSetIter.next().getValue().iterator();
            }

            SignalIndication ret = curr.next();

            if (!curr.hasNext()) {
                curr = null;
            }

            return ret;
        }

        /**
         * Throws an error if the user tries to remove items from the iterator
         * 
         * @throws UnsupportedOperationException
         */
        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}