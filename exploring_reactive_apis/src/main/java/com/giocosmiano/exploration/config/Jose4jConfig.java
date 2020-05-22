package com.giocosmiano.exploration.config;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.compress.utils.ByteUtils;
import org.jose4j.jwa.AlgorithmConstraints;
import org.jose4j.jwe.ContentEncryptionAlgorithmIdentifiers;
import org.jose4j.jwe.JsonWebEncryption;
import org.jose4j.jwe.KeyManagementAlgorithmIdentifiers;
import org.jose4j.jwk.JsonWebKey;
import org.jose4j.jwk.RsaJsonWebKey;
import org.jose4j.jwk.RsaJwkGenerator;
import org.jose4j.jwt.JwtClaims;
import org.jose4j.jwt.NumericDate;
import org.jose4j.jwt.consumer.ErrorCodes;
import org.jose4j.jwt.consumer.InvalidJwtException;
import org.jose4j.jwt.consumer.JwtConsumer;
import org.jose4j.jwt.consumer.JwtConsumerBuilder;
import org.jose4j.keys.AesKey;
import org.jose4j.keys.RsaKeyUtil;
import org.jose4j.lang.ByteUtil;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Calendar;
import java.util.UUID;

// https://bitbucket.org/b_c/jose4j/wiki/Home (This is what's being used in this code)
// https://stormpath.com/blog/jjwt-how-it-works-why
// https://stormpath.com/blog/beginners-guide-jwts-in-java
// https://www.baeldung.com/java-json-web-tokens-jjwt
// https://developer.okta.com/blog/2018/10/31/jwts-with-java
@Log4j2
public class Jose4jConfig {

    public static final String JWS_BODY = "jwsBody";
    public static final String SAMPLE_SUBJECT = "sampleSubject";
    public static final String SAMPLE_ISSUER = "sampleIssuer";
    public static final String SAMPLE_AUDIENCE = "sampleAudience";

    private static JsonWebEncryption createJsonWebEncryption(final String secretKey) {
        // Create a new Json Web Encryption object, using 128bit key size (16 bytes)
        byte[] keyByte = secretKey.getBytes(StandardCharsets.UTF_8);
        if (keyByte.length > 16) {
            keyByte = ByteUtil.subArray(secretKey.getBytes(StandardCharsets.UTF_8), 0, 16);
        }

//        Key key = new AesKey(secretKey.getBytes(StandardCharsets.UTF_8)); // secretKey that only admin should know
        Key key = new AesKey(keyByte); // secretKey that only admin should know
        JsonWebEncryption jsonWebEncryption = new JsonWebEncryption();
        jsonWebEncryption.setKey(key);

        return jsonWebEncryption;
    }

    /**
     *
     * @param secretKey secretKey that only admin should know
     * @param configs object that contains the configuration settings
     * @param <T> class/type of the configuration settings
     * @return String - the encoded JWE
     */
    public static <T> String generateJweConfigs(final String secretKey, final T configs) {
        String encryptedConfigSettings = null;

        try {
            JsonWebEncryption jsonWebEncryption = createJsonWebEncryption(secretKey);

            // if using A258KW, use 256bit key size (32 bytes)
            jsonWebEncryption.setAlgorithmHeaderValue(KeyManagementAlgorithmIdentifiers.A128KW);
            jsonWebEncryption.setEncryptionMethodHeaderParameter(ContentEncryptionAlgorithmIdentifiers.AES_128_CBC_HMAC_SHA_256);

            ObjectMapper objectMapper = new ObjectMapper();
            jsonWebEncryption.setPayload(objectMapper.writeValueAsString(configs));

            encryptedConfigSettings = jsonWebEncryption.getCompactSerialization();

            log.info("encrypted JWE configs: {}", encryptedConfigSettings);

        } catch (Exception e) {
            log.error(e.getMessage());
        }

        return encryptedConfigSettings;
    }

    /**
     *
     * @param secretKey secretKey that only admin should know
     * @param jweConfigs encoded JWE string that will be decoded
     * @param clazz class referencing the type of the configuration settings
     * @param <T> class/type of the configuration settings
     * @return instance of configuration settings
     */
    public static <T> T parseJweConfigs(final String secretKey, final String jweConfigs, final Class<T> clazz) {
        T decryptedConfigSettings = null;

        try {
            JsonWebEncryption jsonWebEncryption = createJsonWebEncryption(secretKey);

            // if using A258KW, use 256bit key size (32 bytes)
            AlgorithmConstraints algorithmConstraintsKMAI =
                    new AlgorithmConstraints(
                            AlgorithmConstraints.ConstraintType.WHITELIST
                            , KeyManagementAlgorithmIdentifiers.A128KW);
            jsonWebEncryption.setAlgorithmConstraints(algorithmConstraintsKMAI);
            AlgorithmConstraints algorithmConstraintsCEAI =
                    new AlgorithmConstraints(
                            AlgorithmConstraints.ConstraintType.WHITELIST
                            , ContentEncryptionAlgorithmIdentifiers.AES_128_CBC_HMAC_SHA_256);
            jsonWebEncryption.setContentEncryptionAlgorithmConstraints(algorithmConstraintsCEAI);
            jsonWebEncryption.setCompactSerialization(jweConfigs);

            String decryptedJweConfigs = jsonWebEncryption.getPlaintextString();
            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            decryptedConfigSettings = objectMapper.readValue(decryptedJweConfigs, clazz);

            log.info("decrypted JWE configs: {}", decryptedJweConfigs);

        } catch (Exception e) {
            log.error(e.getMessage());
        }

        return decryptedConfigSettings;
    }
}