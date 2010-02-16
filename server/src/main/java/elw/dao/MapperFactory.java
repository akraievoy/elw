package elw.dao;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;

public class MapperFactory {
	
	public static ObjectMapper createMapper() {
		final ObjectMapper mapper = new ObjectMapper();

		mapper.getSerializationConfig().enable(SerializationConfig.Feature.INDENT_OUTPUT);

		return mapper;
	}
}
