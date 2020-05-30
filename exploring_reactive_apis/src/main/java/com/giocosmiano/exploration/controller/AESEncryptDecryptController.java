package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.service.AESEncryptDecryptService;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@Log4j2
@RestController
@RequestMapping(value = "/aes")
public class AESEncryptDecryptController {

    private final AESEncryptDecryptService aesEncryptDecryptService;

    public AESEncryptDecryptController(AESEncryptDecryptService aesEncryptDecryptService) {
        this.aesEncryptDecryptService = aesEncryptDecryptService;
    }

    // http://localhost:9080/aes/encrypt?plainText=helloWorld
    // returns --> XUOSDybGJtirLHcEAyaGlHgdJ1ml2ismoAjU6ToZSp35ALL29Y+/ofHyIuqVrX7c
    // http://localhost:9080/aes/encrypt?plainText=th3Qu1ckBr0wnF0xJ8mp$0v3rTh3L8zyD0g
    // returns --> ahyhEPEFYo+fJRGM0We8BL+dhUdjH43HcxCfT2ISNCIFS/KwGWPpfpkt3aL4sVFGh1AIdRYWQyUMep6Eq0bZHS07j2azb0h9pKlE+LUr3lY=
    @GetMapping(value = "/encrypt", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<String> encrypt(@RequestParam String plainText) {
        return Mono.fromCallable(() -> aesEncryptDecryptService.encrypt(plainText));
    }

    // http://localhost:9080/aes/decrypt?cipherText=XUOSDybGJtirLHcEAyaGlHgdJ1ml2ismoAjU6ToZSp35ALL29Y%2B%2FofHyIuqVrX7c
    // returns --> helloWorld
    // http://localhost:9080/aes/decrypt?cipherText=ahyhEPEFYo%2BfJRGM0We8BL%2BdhUdjH43HcxCfT2ISNCIFS%2FKwGWPpfpkt3aL4sVFGh1AIdRYWQyUMep6Eq0bZHS07j2azb0h9pKlE%2BLUr3lY%3D
    // returns --> th3Qu1ckBr0wnF0xJ8mp$0v3rTh3L8zyD0g
    @GetMapping(value = "/decrypt", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<String> decrypt(@RequestParam String cipherText) {
        return Mono.fromCallable(() -> aesEncryptDecryptService.decrypt(cipherText));
    }
}
