package entropy.Utils;

import eta.runtime.io.MemoryManager;
import java.security.SecureRandom;
import java.nio.ByteBuffer;

public class Utils {

    public static int get_rand_bytes(long address, int n){
        ByteBuffer buffer = MemoryManager.getBoundedBuffer(address);
        buffer.put(randomByteArray(n));
        return 0;
    }

    private static byte[] randomByteArray(int n){
        SecureRandom random = new SecureRandom();
        byte[] result = new byte[n];
        random.nextBytes(result);
        return result;
    }

}

