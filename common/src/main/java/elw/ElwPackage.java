package elw;

import elw.vo.IdNamed;
import org.akraievoy.couch.Squab;

import java.util.*;

public class ElwPackage {
    public ElwPackage() { /*sealed*/ }

    public static <IN extends IdNamed> Collection<List<IN>> filterDuplicateIds(Collection<IN> fullList) {
        final TreeMap<String, List<IN>> idToList = new TreeMap<String, List<IN>>();

        for (IN in : fullList) {
            if (idToList.containsKey(in.getId())) {
                idToList.get(in.getId()).add(in);
            } else {
                final List<IN> list = new ArrayList<IN>();
                list.add(in);
                idToList.put(in.getId(), list);
            }
        }

        for (Iterator<String> idIt = idToList.keySet().iterator(); idIt.hasNext(); ) {
            if (idToList.get(idIt.next()).size() == 1) {
                idIt.remove();
            }
        }

        return idToList.values();
    }

    public static Collection<String> mapToCouchIds(Collection<? extends Squab> squabs) {
        final List<String> couchIds = new ArrayList<String>(squabs.size());
        for (Squab squab : squabs) {
            couchIds.add(squab.getCouchId());
        }
        return couchIds;
    }

    public static <IN extends IdNamed> Map<String, IN> groupById(Collection<IN> ins) {
        final TreeMap<String, IN> idToIn = new TreeMap<String, IN>();
        for (IN in : ins) {
            idToIn.put(in.getId(), in);
        }
        return idToIn;
    }

}
