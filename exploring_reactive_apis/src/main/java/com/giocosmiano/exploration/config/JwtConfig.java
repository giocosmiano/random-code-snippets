package com.giocosmiano.exploration.config;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.jsonwebtoken.*;
import io.jsonwebtoken.impl.DefaultClaims;
import org.jasypt.util.text.AES256TextEncryptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Calendar;
import java.util.UUID;

// https://stormpath.com/blog/jjwt-how-it-works-why
// https://stormpath.com/blog/beginners-guide-jwts-in-java
// https://www.baeldung.com/java-json-web-tokens-jjwt
// https://developer.okta.com/blog/2018/10/31/jwts-with-java
public class JwtConfig {

    public static final String JWS_BODY = "jwsBody";
    public static final String SAMPLE_SUBJECT = "sampleSubject";
    public static final String SAMPLE_ISSUER = "sampleIssuer";
    public static final String SAMPLE_AUDIENCE = "sampleAudience";
    protected static final Logger log = LoggerFactory.getLogger(JwtConfig.class);

    /**
     *
     * @param secretKey secretKey that only admin should know
     * @param jwtConfigs object that contains the configuration settings
     * @param <T> class/type of the configuration settings
     * @return String - the encoded JWT
     */
    public static <T> String generateJwtConfigs(final String secretKey, final T jwtConfigs) {
        String encryptedConfigSettings = null;

        try {
            byte[] keyBytes = secretKey.getBytes(StandardCharsets.UTF_8); // secretKey that only admin should know
            SignatureAlgorithm signatureAlgorithm = SignatureAlgorithm.HS256; // using HMAC SHA-256 encryption
            Key signingKey = new SecretKeySpec(keyBytes, signatureAlgorithm.getJcaName());

            ObjectMapper objectMapper = new ObjectMapper();
            String jwsBody = objectMapper.writeValueAsString(jwtConfigs);

            AES256TextEncryptor aes256TextEncryptor = new AES256TextEncryptor();
            aes256TextEncryptor.setPassword(secretKey);
            String encryptedJwsBody = aes256TextEncryptor.encrypt(jwsBody);

            Calendar date = Calendar.getInstance();
            date.add(Calendar.DATE, 1);

            DefaultClaims defaultClaims = new DefaultClaims();
            defaultClaims.setId(UUID.randomUUID().toString());
            defaultClaims.setSubject(SAMPLE_SUBJECT);
            defaultClaims.setAudience(SAMPLE_AUDIENCE);
            defaultClaims.setIssuer(SAMPLE_ISSUER);
            defaultClaims.setIssuedAt(Calendar.getInstance().getTime());
            defaultClaims.setExpiration(date.getTime()); // valid for 1-day
            defaultClaims.put(JWS_BODY, encryptedJwsBody);
            encryptedConfigSettings =
                    Jwts.builder()
                            .setClaims(defaultClaims)
                            .signWith(signingKey)
                            .compact();

            log.info("jwsBody: {}", encryptedConfigSettings);

        } catch (Exception e) {
            log.error(e.getMessage());
        }

        return encryptedConfigSettings;
    }

    /**
     *
     * @param secretKey secretKey that only admin should know
     * @param jwtConfigs encoded JWT string that will be decoded
     * @param clazz class referencing the type of the configuration settings
     * @param <T> class/type of the configuration settings
     * @return instance of configuration settings
     */
    public static <T> T parseJwtConfigs(final String secretKey, final String jwtConfigs, final Class<T> clazz) {
        T decryptedConfigSettings = null;

        try {
            byte[] keyBytes = secretKey.getBytes(StandardCharsets.UTF_8); // secretKey that only admin should know
            SignatureAlgorithm signatureAlgorithm = SignatureAlgorithm.HS256; // using HMAC SHA-256 encryption
            Key signingKey = new SecretKeySpec(keyBytes, signatureAlgorithm.getJcaName());

            Jws<Claims> jwsClaims =
                    Jwts.parserBuilder()
                            .setSigningKey(signingKey)
                            .build()
                            .parseClaimsJws(jwtConfigs);

            JwsHeader jwsHeader = jwsClaims.getHeader();
            String jwsPayloadStr = jwsClaims.getBody().toString();
            DefaultClaims defaultClaims = (DefaultClaims) jwsClaims.getBody();

            log.info("jwsHeader: {}", jwsHeader.toString());
            log.info("jwsPayload: {}", jwsPayloadStr);

            String encryptedJwsBody = (String)defaultClaims.get(JWS_BODY);

            AES256TextEncryptor aes256TextEncryptor = new AES256TextEncryptor();
            aes256TextEncryptor.setPassword(secretKey);
            String jwsBody = aes256TextEncryptor.decrypt(encryptedJwsBody);

            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            decryptedConfigSettings = objectMapper.readValue(jwsBody, clazz);

            log.info("jwsId: {}", defaultClaims.getId());
            log.info("jwsSubject: {}", defaultClaims.getSubject());
            log.info("jwsAudience: {}", defaultClaims.getAudience());
            log.info("jwsIssuer: {}", defaultClaims.getIssuer());
            log.info("jwsIssuedAt: {}", defaultClaims.getIssuedAt());
            log.info("jwsExpiration: {}", defaultClaims.getExpiration());
            log.info("jwsBody: {}", jwsBody);

        } catch (Exception e) {
            log.error(e.getMessage());
        }

        return decryptedConfigSettings;
    }
}