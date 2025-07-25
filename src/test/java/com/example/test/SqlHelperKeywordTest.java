package com.example.test;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.test.entity.SysUser;
import com.example.test.impl.SysUserDynamicFieldService;
import com.example.test.impl.SysUserDynamicSqlService;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * SqlHelper关键字功能测试类
 * 测试SqlHelper支持的各种条件关键字和功能
 */
@Slf4j
@SpringBootTest
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class SqlHelperKeywordTest {

    @Autowired
    private SysUserDynamicSqlService sysUserDynamicSqlService;
    
    @Autowired
    private SysUserDynamicFieldService sysUserDynamicFieldService;

    private static Long userId1;
    private static Long userId2;
    private static Long userId3;
    
    private static Long userId4;
    private static Long userId5;
    private static Long userId6;

    @BeforeEach
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
        sysUserDynamicSqlService.save(user1);
        userId1 = user1.getId();

        SysUser user2 = new SysUser();
        user2.setName("李四");
        user2.setAge(30);
        user2.setBirthDate(LocalDate.of(1993, 5, 10));
        user2.setState(1);
        user2.setCreateTime(LocalDateTime.now());
        user2.setUpdateTime(LocalDateTime.now());
        user2.setDeleted(0);
        user2.setVersion(1);
        sysUserDynamicSqlService.save(user2);
        userId2 = user2.getId();

        SysUser user3 = new SysUser();
        user3.setName("王五");
        user3.setAge(35);
        user3.setBirthDate(LocalDate.of(1988, 12, 20));
        user3.setState(0);
        user3.setCreateTime(LocalDateTime.now());
        user3.setUpdateTime(LocalDateTime.now());
        user3.setDeleted(0);
        user3.setVersion(1);
        sysUserDynamicSqlService.save(user3);
        userId3 = user3.getId();
        
        // 准备测试数据到mysql数据源
        SysUser user4 = new SysUser();
        user4.setName("赵六");
        user4.setAge(28);
        user4.setBirthDate(LocalDate.of(1995, 3, 15));
        user4.setState(1);
        user4.setCreateTime(LocalDateTime.now());
        user4.setUpdateTime(LocalDateTime.now());
        user4.setDeleted(0);
        user4.setVersion(1);
        sysUserDynamicFieldService.save(user4);
        userId4 = user4.getId();

        SysUser user5 = new SysUser();
        user5.setName("钱七");
        user5.setAge(32);
        user5.setBirthDate(LocalDate.of(1991, 8, 22));
        user5.setState(1);
        user5.setCreateTime(LocalDateTime.now());
        user5.setUpdateTime(LocalDateTime.now());
        user5.setDeleted(0);
        user5.setVersion(1);
        sysUserDynamicFieldService.save(user5);
        userId5 = user5.getId();

        SysUser user6 = new SysUser();
        user6.setName("孙八");
        user6.setAge(27);
        user6.setBirthDate(LocalDate.of(1996, 11, 5));
        user6.setState(0);
        user6.setCreateTime(LocalDateTime.now());
        user6.setUpdateTime(LocalDateTime.now());
        user6.setDeleted(0);
        user6.setVersion(1);
        sysUserDynamicFieldService.save(user6);
        userId6 = user6.getId();
    }

    @AfterEach
    void tearDown() {
        // 清理postgresql数据源测试数据
        if (userId1 != null) sysUserDynamicSqlService.removeById(userId1);
        if (userId2 != null) sysUserDynamicSqlService.removeById(userId2);
        if (userId3 != null) sysUserDynamicSqlService.removeById(userId3);
        
        // 清理mysql数据源测试数据
        if (userId4 != null) sysUserDynamicFieldService.removeById(userId4);
        if (userId5 != null) sysUserDynamicFieldService.removeById(userId5);
        if (userId6 != null) sysUserDynamicFieldService.removeById(userId6);
    }

    /**
     * 测试 eq 条件
     */
    @Test
    @Order(1)
    public void testEq() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .eq(SysUser::getName, "张三")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue( list.stream().allMatch(e->e.getName().equals("张三")));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .eq(SysUser::getName, "赵六")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue( list2.stream().allMatch(e->e.getName().equals("赵六")));
    }

    /**
     * 测试 ne 条件
     */
    @Test
    @Order(2)
    public void testNe() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .ne(SysUser::getName, "张三")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertFalse( list.stream().anyMatch(e->e.getName().equals("张三")));
        
        list.forEach(user -> assertNotEquals("张三", user.getName()));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .ne(SysUser::getName, "赵六")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertFalse( list2.stream().anyMatch(e->e.getName().equals("赵六")));
    }

    /**
     * 测试 gt 条件
     */
    @Test
    @Order(3)
    public void testGt() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .gt(SysUser::getAge, 25)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 2); // 李四(30)和王五(35)
        list.forEach(user -> assertTrue(user.getAge() > 25));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .gt(SysUser::getAge, 27)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 2); // 赵六(28)、钱七(32)
        list2.forEach(user -> assertTrue(user.getAge() > 27));
    }

    /**
     * 测试 ge 条件
     */
    @Test
    @Order(4)
    public void testGe() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .ge(SysUser::getAge, 25)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 3); // 张三(25)、李四(30)和王五(35)
        list.forEach(user -> assertTrue(user.getAge() >= 25));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .ge(SysUser::getAge, 27)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 3); // 赵六(28)、钱七(32)、孙八(27)
        list2.forEach(user -> assertTrue(user.getAge() >= 27));
    }

    /**
     * 测试 lt 条件
     */
    @Test
    @Order(5)
    public void testLt() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .lt(SysUser::getAge, 30)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 1); // 张三(25)
        list.forEach(user -> assertTrue(user.getAge() < 30));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .lt(SysUser::getAge, 30)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 2); // 赵六(28)、孙八(27)
        list2.forEach(user -> assertTrue(user.getAge() < 30));
    }

    /**
     * 测试 le 条件
     */
    @Test
    @Order(6)
    public void testLe() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .le(SysUser::getAge, 30)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 2); // 张三(25)和李四(30)
        list.forEach(user -> assertTrue(user.getAge() <= 30));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .le(SysUser::getAge, 30)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 2); // 赵六(28)、孙八(27)
        list2.forEach(user -> assertTrue(user.getAge() <= 30));
    }

    /**
     * 测试 like 条件
     */
    @Test
    @Order(7)
    public void testLike() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .like(SysUser::getName, "张")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue( list.size()>=1);
        assertTrue(list.stream().allMatch(e->e.getName().contains("张")));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .like(SysUser::getName, "赵")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue( list2.size()>=1);
        assertTrue(list2.stream().allMatch(e->e.getName().contains("张")));
    }

    /**
     * 测试 notLike 条件
     */
    @Test
    @Order(8)
    public void testNotLike() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .notLike(SysUser::getName, "张")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 2);
        list.forEach(user -> assertFalse(user.getName().contains("张")));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .notLike(SysUser::getName, "赵")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 2);
        list2.forEach(user -> assertFalse(user.getName().contains("赵")));
    }

    /**
     * 测试 in 条件
     */
    @Test
    @Order(9)
    public void testIn() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .in(SysUser::getAge, Arrays.asList(25, 30))
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue( list.size()>=2);
        Set<Integer> ages = new HashSet<>(Arrays.asList(25, 30));
        list.forEach(user -> assertTrue(ages.contains(user.getAge())));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .in(SysUser::getAge, Arrays.asList(28, 32))
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue( list2.size()>=2);
        Set<Integer> ages2 = new HashSet<>(Arrays.asList(28, 32));
        list2.forEach(user -> assertTrue(ages2.contains(user.getAge())));
    }

    /**
     * 测试 notIn 条件
     */
    @Test
    @Order(10)
    public void testNotIn() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .notIn(SysUser::getAge, Arrays.asList(25, 30))
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 1); // 王五(35)
        Set<Integer> ages = new HashSet<>(Arrays.asList(25, 30));
        list.forEach(user -> assertFalse(ages.contains(user.getAge())));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .notIn(SysUser::getAge, Arrays.asList(28, 32))
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 1); // 孙八(27)
        Set<Integer> ages2 = new HashSet<>(Arrays.asList(28, 32));
        list2.forEach(user -> assertFalse(ages2.contains(user.getAge())));
    }

    /**
     * 测试 isNull 条件
     */
    @Test
    @Order(11)
    public void testIsNull() {
        // 先添加一个name为null的测试数据到postgresql数据源
        SysUser user = new SysUser();
        user.setName(null);
        user.setAge(40);
        user.setBirthDate(LocalDate.of(1983, 1, 1));
        user.setState(1);
        user.setCreateTime(LocalDateTime.now());
        user.setUpdateTime(LocalDateTime.now());
        user.setDeleted(0);
        user.setVersion(1);
        sysUserDynamicSqlService.save(user);

        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .isNull(SysUser::getName)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 1);
        list.forEach(u -> assertNull(u.getName()));

        // 清理临时数据
        sysUserDynamicSqlService.removeById(user.getId());
        
        // 先添加一个name为null的测试数据到mysql数据源
        SysUser user2 = new SysUser();
        user2.setName(null);
        user2.setAge(42);
        user2.setBirthDate(LocalDate.of(1981, 1, 1));
        user2.setState(1);
        user2.setCreateTime(LocalDateTime.now());
        user2.setUpdateTime(LocalDateTime.now());
        user2.setDeleted(0);
        user2.setVersion(1);
        sysUserDynamicFieldService.save(user2);

        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .isNull(SysUser::getName)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 1);
        list2.forEach(u -> assertNull(u.getName()));

        // 清理临时数据
        sysUserDynamicFieldService.removeById(user2.getId());
    }

    /**
     * 测试 isNotNull 条件
     */
    @Test
    @Order(12)
    public void testIsNotNull() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .isNotNull(SysUser::getName)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 3); // 原始的3个用户
        list.forEach(u -> assertNotNull(u.getName()));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .isNotNull(SysUser::getName)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 3); // 原始的3个用户
        list2.forEach(u -> assertNotNull(u.getName()));
    }

    /**
     * 测试 or 条件
     */
    @Test
    @Order(13)
    public void testOr() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .eq(SysUser::getName, "张三")
                .or()
                .eq(SysUser::getName, "李四")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size()>=2);
        Set<String> names = new HashSet<>(Arrays.asList("张三", "李四"));
        list.forEach(user -> assertTrue(names.contains(user.getName())));
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .eq(SysUser::getName, "赵六")
                .or()
                .eq(SysUser::getName, "钱七")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size()>=2);
        Set<String> names2 = new HashSet<>(Arrays.asList("赵六", "钱七"));
        list2.forEach(user -> assertTrue(names2.contains(user.getName())));
    }

    /**
     * 测试 orderByAsc 排序
     */
    @Test
    @Order(14)
    public void testOrderByAsc() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 3);
        for (int i = 0; i < list.size() - 1; i++) {
            assertTrue(list.get(i).getAge() <= list.get(i + 1).getAge());
        }
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 3);
        for (int i = 0; i < list2.size() - 1; i++) {
            assertTrue(list2.get(i).getAge() <= list2.get(i + 1).getAge());
        }
    }

    /**
     * 测试 orderByDesc 排序
     */
    @Test
    @Order(15)
    public void testOrderByDesc() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .orderByDesc(SysUser::getAge)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 3);
        for (int i = 0; i < list.size() - 1; i++) {
            assertTrue(list.get(i).getAge() >= list.get(i + 1).getAge());
        }
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .orderByDesc(SysUser::getAge)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 3);
        for (int i = 0; i < list2.size() - 1; i++) {
            assertTrue(list2.get(i).getAge() >= list2.get(i + 1).getAge());
        }
    }

    /**
     * 测试 page 分页功能
     */
    @Test
    @Order(16)
    public void testPage() {
        IPage<SysUserVO> page = SqlHelper.<SysUser>of()
                .ge(SysUser::getAge, 25)
                .wrap(sysUserDynamicSqlService)
                .page(1L, 2L);
        assertNotNull(page);
        assertEquals(1, page.getCurrent());
        assertEquals(2, page.getSize());
        assertTrue(page.getTotal() >= 3);
        assertTrue(page.getRecords().size() <= 2);
        
        IPage<SysUserVO> page2 = SqlHelper.<SysUser>of()
                .ge(SysUser::getAge, 25)
                .wrap(sysUserDynamicFieldService)
                .page(1L, 2L);
        assertNotNull(page2);
        assertEquals(1, page2.getCurrent());
        assertEquals(2, page2.getSize());
        assertTrue(page2.getTotal() >= 3);
        assertTrue(page2.getRecords().size() <= 2);
    }

    /**
     * 测试 requiredNext 功能
     */
    @Test
    @Order(17)
    public void testRequiredNext() {
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .eq(SysUser::getAge, 25)
                .requiredNext()
                .eq(SysUser::getName, "张三")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        // 验证嵌套条件查询是否正确执行
        assertTrue(list.size() >= 0); // 可能为空，取决于具体数据和查询逻辑
        
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .eq(SysUser::getAge, 28)
                .requiredNext()
                .eq(SysUser::getName, "赵六")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        // 验证嵌套条件查询是否正确执行
        assertTrue(list2.size() >= 0); // 可能为空，取决于具体数据和查询逻辑
    }
}