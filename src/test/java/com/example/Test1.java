package com.example;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.test.entity.SysUser;
import com.example.test.impl.PostgresEnhancedService;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author bootystar
 */
@Slf4j
@SpringBootTest
public class Test1 {

    @Autowired
    private PostgresEnhancedService postgresService;

    private static Long userId1;
    private static Long userId2;
    private static Long userId3;

//    @Test
//    @Order(0)
    void setUp() {
        // 准备测试数据到postgresql数据源
        SysUser user1 = new SysUser();
        user1.setName("张三");
        user1.setAge(25);
        user1.setBirthDate(LocalDate.of(1998, 1, 1));
        user1.setState(1);
        user1.setCreateTime(LocalDateTime.now());
        user1.setUpdateTime(LocalDateTime.now());
        user1.setDeleted(0);
        user1.setVersion(1);
        postgresService.save(user1);
        userId1 = user1.getId();

        SysUser user2 = new SysUser();
        user2.setName("李四");
        user2.setAge(30);
        user2.setBirthDate(LocalDate.of(1993, 5, 10));
        user2.setState(2);
        user2.setCreateTime(LocalDateTime.now());
        user2.setUpdateTime(LocalDateTime.now());
        user2.setDeleted(0);
        user2.setVersion(1);
        postgresService.save(user2);
        userId2 = user2.getId();

        SysUser user3 = new SysUser();
        user3.setName("王五");
        user3.setAge(35);
        user3.setBirthDate(LocalDate.of(1988, 12, 20));
        user3.setState(3);
        user3.setCreateTime(LocalDateTime.now());
        user3.setUpdateTime(LocalDateTime.now());
        user3.setDeleted(0);
        user3.setVersion(1);
        postgresService.save(user3);
        userId3 = user3.getId();


        // 先添加一个name为null的测试数据到postgresql数据源
        SysUser user = new SysUser();
        user.setName(null);
        user.setAge(40);
        user.setBirthDate(LocalDate.of(1983, 1, 1));
        user.setState(4);
        user.setCreateTime(LocalDateTime.now());
        user.setUpdateTime(LocalDateTime.now());
        user.setDeleted(0);
        user.setVersion(1);
        postgresService.save(user);
    }

    //    @Test
//    @Order(Integer.MAX_VALUE)
    void tearDown() {
        // 清理postgresql数据源测试数据
        if (userId1 != null) postgresService.removeById(userId1);
        if (userId2 != null) postgresService.removeById(userId2);
        if (userId3 != null) postgresService.removeById(userId3);
    }

    /**
     * 测试 eq 条件
     */
    @Test
    @Order(1)
    public void testEq() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .eq(SysUser::getName, "张三")
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        assertTrue(list.stream().allMatch(e -> e.getName().equals("张三")));
    }

    /**
     * 测试 ne 条件
     */
    @Test
    @Order(2)
    public void testNe() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .ne(SysUser::getName, "张三")
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        assertFalse(list.stream().anyMatch(e -> e.getName().equals("张三")));
    }

    /**
     * 测试 gt 条件
     */
    @Test
    @Order(3)
    public void testGt() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .gt(SysUser::getAge, 25)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty()); // 李四(30)和王五(35)
        list.forEach(user -> assertTrue(user.getAge() > 25));
    }

    /**
     * 测试 ge 条件
     */
    @Test
    @Order(4)
    public void testGe() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 25)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty()); // 张三(25)、李四(30)和王五(35)
        list.forEach(user -> assertTrue(user.getAge() >= 25));
    }

    /**
     * 测试 lt 条件
     */
    @Test
    @Order(5)
    public void testLt() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .lt(SysUser::getAge, 30)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty()); // 张三(25)
        list.forEach(user -> assertTrue(user.getAge() < 30));
    }

    /**
     * 测试 le 条件
     */
    @Test
    @Order(6)
    public void testLe() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .le(SysUser::getAge, 30)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty()); // 张三(25)和李四(30)
        list.forEach(user -> assertTrue(user.getAge() <= 30));
    }

    /**
     * 测试 like 条件
     */
    @Test
    @Order(7)
    public void testLike() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .like(SysUser::getName, "张")
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        assertTrue(list.get(0).getName().contains("张"));
    }

    /**
     * 测试 notLike 条件
     */
    @Test
    @Order(8)
    public void testNotLike() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .notLike(SysUser::getName, "张")
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        list.forEach(user -> assertFalse(user.getName().contains("张")));
    }

    /**
     * 测试 in 条件
     */
    @Test
    @Order(9)
    public void testIn() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .in(SysUser::getAge, Arrays.asList(25, 30))
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        Set<Integer> ages = new HashSet<>(Arrays.asList(25, 30));
        list.forEach(user -> assertTrue(ages.contains(user.getAge())));
    }

    /**
     * 测试 notIn 条件
     */
    @Test
    @Order(10)
    public void testNotIn() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .notIn(SysUser::getAge, Arrays.asList(25, 30))
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 0); // 王五(35)
        Set<Integer> ages = new HashSet<>(Arrays.asList(25, 30));
        list.forEach(user -> assertFalse(ages.contains(user.getAge())));
    }

    /**
     * 测试 isNull 条件
     */
    @Test
    @Order(11)
    public void testIsNull() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .isNull(SysUser::getName)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        list.forEach(u -> assertNull(u.getName()));
        
    }

    /**
     * 测试 isNotNull 条件
     */
    @Test
    @Order(12)
    public void testIsNotNull() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .isNotNull(SysUser::getName)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty()); // 原始的3个用户
        list.forEach(u -> assertNotNull(u.getName()));
    }

    /**
     * 测试 or 条件
     */
    @Test
    @Order(13)
    public void testOr() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .or(s -> s.eq(SysUser::getName, "张三").eq(SysUser::getName, "李四"))
//                .or(s -> s.le(SysUser::getAge, 35).ge(SysUser::getAge, 20))
                .or(s -> s.le(SysUser::getAge, 35).ge(SysUser::getAge, 20))
//                .or(s -> s.with(null))
                .eq(SysUser::getState, 1)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        Set<String> names = new HashSet<>(Arrays.asList("张三", "李四"));
        list.forEach(user -> assertTrue(names.contains(user.getName())));
    }

    /**
     * 测试 orderByAsc 排序
     */
    @Test
    @Order(14)
    public void testOrderByAsc() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .orderByAsc(SysUser::getAge)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        for (int i = 0; i < list.size() - 1; i++) {
            assertTrue(list.get(i).getAge() <= list.get(i + 1).getAge());
        }
    }

    /**
     * 测试 orderByDesc 排序
     */
    @Test
    @Order(15)
    public void testOrderByDesc() {
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .orderByDesc(SysUser::getAge)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertFalse(list.isEmpty());
        for (int i = 0; i < list.size() - 1; i++) {
            assertTrue(list.get(i).getAge() >= list.get(i + 1).getAge());
        }
    }

    /**
     * 测试 page 分页功能
     */
    @Test
    @Order(16)
    public void testPage() {
        IPage<SysUserVO> page = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 25)
                .wrap(postgresService)
                .page(1L, 2L);
        assertNotNull(page);
        assertEquals(1, page.getCurrent());
        assertEquals(2, page.getSize());
        assertTrue(page.getTotal() >= 1);
        assertTrue(page.getRecords().size() >= 1);
    }

    /**
     * 测试 requiredNext 功能
     */
    @Test
    @Order(17)
    public void testRequiredNext() {
        SqlHelper<SysUser> child = SqlHelper.of(SysUser.class).eq(SysUser::getName, "张三");
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .eq(SysUser::getAge, 25)
                .with(child)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        // 验证嵌套条件查询是否正确执行
        assertTrue(list.size() >= 0); // 可能为空，取决于具体数据和查询逻辑
    }


    @Test
    @Order(18)
    public void testWithBit() {
        int bit = 1;
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .bitwiseWith(SysUser::getState, bit)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertTrue(list.stream().allMatch(user -> (user.getState() & bit) > 0));
    }

    @Test
    @Order(19)
    public void testWithoutBit() {
        int bit = 1;
        List<SysUserVO> list = SqlHelper.of(SysUser.class)
                .bitwiseWithout(SysUser::getState, bit)
                .wrap(postgresService)
                .list();
        assertNotNull(list);
        assertTrue(list.stream().allMatch(user -> (user.getState() & bit) == 0));
    }
    
    
    @Test
    @Order(20)
    @SneakyThrows
    public void testExcelExport() {
        File file = new File("test.xlsx");
        System.out.println(file.getAbsolutePath());
        if (file.exists()){
            file.delete();
        }
//        file.deleteOnExit();
        FileOutputStream fileOutputStream = new FileOutputStream(file);
        postgresService.excelExport(null,fileOutputStream,SysUserVO.class);
       
    }
    
    @Test
    @Order(21)
    @SneakyThrows
    public void testExcelImport() {
        File file = new File("test.xlsx");
        System.out.println(file.getAbsolutePath());
        FileInputStream fileInputStream = new FileInputStream(file);
        postgresService.excelImport(fileInputStream,SysUserVO.class);
    }
    
}
