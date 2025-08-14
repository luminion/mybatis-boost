package com.example;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.example.test.entity.SysUser;
import com.example.test.impl.MysqlService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * @author bootystar
 */
@SpringBootTest
public class CreateMysqlDataTest {

    @Autowired
    private MysqlService baseService;

    @Test
    void setUp() {
        // 清理之前的测试数据
        baseService.remove(Wrappers.emptyWrapper());

        // 准备测试数据
        SysUser user1 = new SysUser();
        user1.setName("张三");
        user1.setDescription("EnhancedMapper测试用户1");
        user1.setAge(25);
        user1.setBirthDate(LocalDate.of(1998, 1, 1));
        user1.setState(1);
        user1.setCreateTime(LocalDateTime.now());
        user1.setUpdateTime(LocalDateTime.now());
        user1.setDeleted(0);
        user1.setVersion(1);
        baseService.save(user1);

        SysUser user2 = new SysUser();
        user2.setName("李四");
        user2.setDescription("EnhancedMapper测试用户2");
        user2.setAge(30);
        user2.setBirthDate(LocalDate.of(1993, 5, 10));
        user2.setState(2);
        user2.setCreateTime(LocalDateTime.now());
        user2.setUpdateTime(LocalDateTime.now());
        user2.setDeleted(0);
        user2.setVersion(1);
        baseService.save(user2);

        SysUser user3 = new SysUser();
        user3.setName("王五");
        user3.setDescription("EnhancedMapper测试用户3");
        user3.setAge(35);
        user3.setBirthDate(LocalDate.of(1988, 12, 20));
        user3.setState(3);
        user3.setCreateTime(LocalDateTime.now());
        user3.setUpdateTime(LocalDateTime.now());
        user3.setDeleted(0);
        user3.setVersion(1);
        baseService.save(user3);

        SysUser user4 = new SysUser();
        user4.setName(null);
        user4.setDescription("EnhancedMapper测试用户4");
        user4.setAge(40);
        user4.setBirthDate(LocalDate.of(1985, 3, 15));
        user4.setState(4);
        user4.setCreateTime(LocalDateTime.now());
        user4.setUpdateTime(LocalDateTime.now());
        user4.setDeleted(0);
        user4.setVersion(1);
        baseService.save(user4);
    }
    
}
