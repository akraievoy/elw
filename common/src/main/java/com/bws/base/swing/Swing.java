package com.bws.base.swing;

import org.akraievoy.gear.G;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.lang.reflect.*;
import javax.swing.*;

import java.util.*;
import java.util.logging.*;

public class Swing {
    Swing() {
    }

    public static JPanel flow(Component[] components, final int interval) {
        final JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));

        for (Component component : components) {
            result.add(Box.createHorizontalStrut(interval));
            result.add(component);
        }
        result.add(Box.createHorizontalStrut(interval));

        return result;
    }

    public static JPanel flowVert(Component[] components, final int interval) {
        final JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));

        for (Component component : components) {
            result.add(Box.createVerticalStrut(interval));
            result.add(component);
        }
        result.add(Box.createVerticalStrut(interval));

        return result;
    }

    public static JScrollPane scroll(final JComponent viewPort, int prefWidth, int prefHeight) {
        JScrollPane result = new JScrollPane();

        result.setViewportView(viewPort);
        result.setPreferredSize(new Dimension(prefWidth, prefHeight));

        return result;
    }

    public static JComponent border(JComponent component, final String title) {
        component.setBorder(BorderFactory.createTitledBorder(title));

        return component;
    }

    public static Font lookupMonospaceFont() {
        final Font[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts();

        Font courierNew= null;
        Font courier= null;
        Font monospace= null;

        for (Font font : fonts) {
            if ("Courier New".equalsIgnoreCase(font.getName())) {
                courierNew = font;
            }
            if (font.getName().toLowerCase().contains("courier")) {
                courier = font;
            }
            if (font.getFamily().toLowerCase().contains("monospace")) {
                monospace = font;
            }
        }

        final Font toReturn = courierNew != null ? courierNew : courier != null ? courier : monospace;

        return toReturn != null ? toReturn.deriveFont(Font.PLAIN, 12) : Font.decode(null);
    }

    public static class ActionFactory {
        public static final String PREFIX = "do_";

        static final Logger log = Logger.getLogger(ActionFactory.class.getName());

        protected final Object targetObject;
        protected final Map<String, AbstractAction> actions = new HashMap<String, AbstractAction>();

        public ActionFactory(final Object targetObject) {
            this.targetObject = targetObject;
        }

		public ActionFactory init() {
			return init(new Properties());
		}

        public ActionFactory init(Properties actionProps) {
            final Method[] allMethods = targetObject.getClass().getMethods();

            for (Method method : allMethods) {
                final String methodName = method.getName();
                if (methodName.startsWith(PREFIX)) {
                    final Class<?>[] params = method.getParameterTypes();
                    if (params.length == 1 && ActionEvent.class.equals(params[0])) {
                        final String key = methodName.substring(PREFIX.length());
                        create(key, actionProps.getProperty(targetObject.getClass().getName() + "." + key + ".name", key));
                    }
                }
            }

            return this;    //  allow for call chaining
        }

        protected AbstractAction create(String methodKey, String name) {
            final String methName = PREFIX + methodKey;

            final Method method;
            try {
                method = targetObject.getClass().getMethod(methName, ActionEvent.class);
            } catch (NoSuchMethodException e) {
                throw base.Die.criticalConfigError("'" + targetObject.getClass() + "." + methName + "' not found");
            }

            final AbstractAction newAction = new AbstractAction(name) {
                public void actionPerformed(ActionEvent e) {
                    try {

                        method.invoke(targetObject, e);

                    } catch (IllegalAccessException e1) {

                        throw base.Die.criticalConfigError("'" + targetObject.getClass() + "." + methName + "' not accessible");

                    } catch (InvocationTargetException ite) {

                        log.log(Level.WARNING, "failed: " + G.report(ite), ite);

                    }
                }
            };

            base.Die.ifNotNull("prev action for '" + methodKey + "'", actions.put(methodKey, newAction));

            return newAction;
        }

        public AbstractAction forKey(final String methodKey) {
            final AbstractAction abstractAction = actions.get(methodKey);

            base.Die.ifNull("action for key '" + methodKey + "'", abstractAction);

            return abstractAction;
        }

        public void enable(final String methodKey) {
            setEnabled(methodKey, true);
        }

        public void disable(final String methodKey) {
            setEnabled(methodKey, false);
        }

        public void setEnabled(final String methodKey, boolean newEnabled) {
            forKey(methodKey).setEnabled(newEnabled);
        }

        public void setName(final String methodKey, String newName) {
            forKey(methodKey).putValue(AbstractAction.NAME, newName);
        }

		public void link(final String methodKey, final AbstractButton button) {
			final AbstractAction linkedAction = forKey(methodKey);

			linkedAction.putValue(AbstractAction.NAME, button.getText());
			linkedAction.putValue(AbstractAction.SMALL_ICON, button.getIcon());
			linkedAction.putValue(AbstractAction.SHORT_DESCRIPTION, button.getToolTipText());

			button.setAction(linkedAction);
		}
    }
}
