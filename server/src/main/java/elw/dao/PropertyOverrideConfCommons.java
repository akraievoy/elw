package elw.dao;

import org.apache.commons.configuration.CompositeConfiguration;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.PropertyOverrideConfigurer;

import java.util.Iterator;
import java.util.Properties;

public class PropertyOverrideConfCommons extends PropertyOverrideConfigurer {
    private String baseName = "myapp";
    private String prefix = "context";

    public void setBaseName(final String baseName) {
        this.baseName = baseName;
    }

    public void setPrefix(final String prefix) {
        this.prefix = prefix;
    }

    @Override
    protected void processProperties(
            ConfigurableListableBeanFactory beanFactoryToProcess,
            Properties props
    ) throws BeansException {
        final CompositeConfiguration compConf =
            ConfigLoader.getConfiguration(baseName);

        if (props == null) {
            props = new Properties();
        }

        for (Iterator<String> keyIt = compConf.getKeys(); keyIt.hasNext();) {
            final String key = keyIt.next();
            if (key.length() <= prefix.length() || !key.startsWith(prefix)) {
                continue;
            }
            
            props.put(
                key.substring(prefix.length()), 
                compConf.getString(key)
            );
        }

        super.processProperties(beanFactoryToProcess, props);
    }

}
