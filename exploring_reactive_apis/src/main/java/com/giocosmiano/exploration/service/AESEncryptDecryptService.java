package com.giocosmiano.exploration.service;

import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.security.AlgorithmParameters;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.util.Arrays;
import java.util.Base64;

// https://stackoverflow.com/questions/44878997/handling-of-iv-and-salt-in-java-encryption-and-decryption
// https://stackoverflow.com/questions/28622438/aes-256-password-based-encryption-decryption-in-java
// https://stackabuse.com/password-encoding-with-spring-security/
// https://www.veracode.com/blog/research/encryption-and-decryption-java-cryptography
@Log4j2
@Service
public class AESEncryptDecryptService {

    @Value("${pwd-encoder.secret-key}")
    private String secretKey;

    @Value("${pwd-encoder.iteration}")
    private int iteration;

    @Value("${pwd-encoder.hash-width}")
    private int hashWidth;

    @Value("${pwd-encoder.salt-length}")
    private int saltLength;

    private static final String AES = "AES";
    private static final String ALGORITHM = "PBKDF2WithHmacSHA256";
    private static final String CIPHER_AES_CBC_PKCS5Padding = "AES/CBC/PKCS5Padding";

    public String encrypt(final String plainText) {

        String cipherText = null;

        SecureRandom sr = new SecureRandom();
        byte[] salt = new byte[saltLength];
        sr.nextBytes(salt);

        try {
            SecretKeySpec secretKeySpec = generateSecretKeySpec(salt);
            Cipher cipher = Cipher.getInstance(CIPHER_AES_CBC_PKCS5Padding);

            cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec);
            AlgorithmParameters params = cipher.getParameters();
            byte[] iv = params.getParameterSpec(IvParameterSpec.class).getIV();
            byte[] encryptedText = cipher.doFinal(plainText.getBytes(StandardCharsets.UTF_8));

            // concatenate salt + iv + ciphertext
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            outputStream.write(salt);
            outputStream.write(iv);
            outputStream.write(encryptedText);

            cipherText = Base64.getEncoder().encodeToString(outputStream.toByteArray());

        } catch (Exception ex) {
            log.error("Error while encrypting: " + ex.getMessage());
        }

        return cipherText;
    }

    public String decrypt(final String cipherText) {

        String plainText = null;

        byte[] decodedCipherText = Base64.getDecoder().decode(cipherText);
        if (decodedCipherText.length < 48) {
            return null;
        }

        byte[] salt = Arrays.copyOfRange(decodedCipherText, 0, 16);
        byte[] iv = Arrays.copyOfRange(decodedCipherText, 16, 32);
        byte[] ct = Arrays.copyOfRange(decodedCipherText, 32, decodedCipherText.length);

        try {
            SecretKeySpec secretKeySpec = generateSecretKeySpec(salt);
            Cipher cipher = Cipher.getInstance(CIPHER_AES_CBC_PKCS5Padding);
            cipher.init(Cipher.DECRYPT_MODE, secretKeySpec, new IvParameterSpec(iv));
            byte[] string = cipher.doFinal(ct);

            plainText = new String(string, StandardCharsets.UTF_8);

        } catch (Exception ex) {
            log.error("Error while decrypting: " + ex.getMessage());
        }

        return plainText;
    }

    private SecretKeySpec generateSecretKeySpec(final byte[] salt) {
        SecretKeySpec genSecretKey = null;
        try {
            SecretKeyFactory factory = SecretKeyFactory.getInstance(ALGORITHM);
            PBEKeySpec spec = new PBEKeySpec(secretKey.toCharArray(), salt, iteration, hashWidth);
            SecretKey tmp = factory.generateSecret(spec);
            genSecretKey = new SecretKeySpec(tmp.getEncoded(), AES);

        } catch (NoSuchAlgorithmException | InvalidKeySpecException ex) {
            log.error("Error while generating secret key: " + ex.getMessage());
        }

        return genSecretKey;
    }
}