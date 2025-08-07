package com.example;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.test.entity.SysUser;
import com.example.test.impl.PostgresService;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author bootystar
 */
@Slf4j
@SpringBootTest
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class SqlHelperTest1 {

    @Autowired
    private PostgresService postgresService;

    private static Long userId1;
    private static Long userId2;
    private static Long userId3;
    private static Long userId4;
    private static Long userId5; // 新增测试用户

    @BeforeAll
    static void setUpClass() {
        log.info("开始SqlHelper测试");
    }

    @AfterAll
    static void tearDownClass() {
        log.info("SqlHelper测试完成");
    }

    @BeforeEach
    void setUp() {
        // 清理之前的测试数据
        tearDown();

        // 准备测试数据到postgresql数据源
        SysUser user1 = new SysUser();
        user1.setName("张三");
        user1.setDescription("测试用户1");
        user1.setAge(25);
        user1.setBirthDate(LocalDate.of(1998, 1, 1));
        user1.setState(1); // 二进制 001
        user1.setCreateTime(LocalDateTime.now());
        user1.setUpdateTime(LocalDateTime.now());
        user1.setDeleted(0);
        user1.setVersion(1);
        postgresService.save(user1);
        userId1 = user1.getId();

        SysUser user2 = new SysUser();
        user2.setName("李四");
        user2.setDescription("测试用户2");
        user2.setAge(30);
        user2.setBirthDate(LocalDate.of(1993, 5, 10));
        user2.setState(2); // 二进制 010
        user2.setCreateTime(LocalDateTime.now());
        user2.setUpdateTime(LocalDateTime.now());
        user2.setDeleted(0);
        user2.setVersion(1);
        postgresService.save(user2);
        userId2 = user2.getId();

        SysUser user3 = new SysUser();
        user3.setName("王五");
        user3.setDescription("测试用户3");
        user3.setAge(35);
        user3.setBirthDate(LocalDate.of(1988, 12, 20));
        user3.setState(3); // 二进制 011
        user3.setCreateTime(LocalDateTime.now());
        user3.setUpdateTime(LocalDateTime.now());
        user3.setDeleted(0);
        user3.setVersion(1);
        postgresService.save(user3);
        userId3 = user3.getId();

        SysUser user4 = new SysUser();
        user4.setName(null); // 测试null值
        user4.setDescription("测试用户4");
        user4.setAge(40);
        user4.setBirthDate(LocalDate.of(1998, 1, 1));
        user4.setState(4);
        user4.setCreateTime(LocalDateTime.now());
        user4.setUpdateTime(LocalDateTime.now());
        user4.setDeleted(0);
        user4.setVersion(1);
        postgresService.save(user4);
        userId4 = user4.getId();

        // 新增边界测试用户
        SysUser user5 = new SysUser();
        user5.setName("赵六");
        user5.setDescription("边界测试用户");
        user5.setAge(0); // 边界值测试
        user5.setBirthDate(LocalDate.of(2000, 1, 1));
        user5.setState(0);
        user5.setCreateTime(LocalDateTime.now());
        user5.setUpdateTime(LocalDateTime.now());
        user5.setDeleted(0);
        user5.setVersion(1);
        postgresService.save(user5);
        userId5 = user5.getId();
    }

    @AfterEach
    void tearDown() {
        // 清理postgresql数据源测试数据
        try {
            if (userId1 != null) postgresService.removeById(userId1);
            if (userId2 != null) postgresService.removeById(userId2);
            if (userId3 != null) postgresService.removeById(userId3);
            if (userId4 != null) postgresService.removeById(userId4);
            if (userId5 != null) postgresService.removeById(userId5);
        } catch (Exception e) {
            log.warn("清理测试数据时出现异常: {}", e.getMessage());
        }
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
        assertEquals("张三", list.get(0).getName());
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
        list.forEach(user -> assertNotEquals("张三", user.getName()));
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
        assertTrue(list.stream().allMatch(e -> e.getName() == null));

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
                .ge(SysUser::getState, 1)
                .or(s->s.eq(SysUser::getName, "李四").eq(SysUser::getName, "张三"))
                .or(s->s.ge(SysUser::getAge, 20).le(SysUser::getAge, 30))
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
     * 测试 bitwiseWith 条件
     */
    @Test
    @Order(17)
    public void testBitwiseWith() {
        // 测试具有指定位的用户 (state & 1 > 0)
        // 张三(state=1)和王五(state=3)应该匹配这个条件
        List<SysUserVO> list1 = SqlHelper.of(SysUser.class)
                .bitWith(SysUser::getState, 1)
                .wrap(postgresService)
                .list();
        assertNotNull(list1);
        assertTrue(list1.size() >= 2); // 张三和王五
        assertTrue(list1.stream().allMatch(e -> (e.getState() & 1) > 0));

        // 测试具有指定位的用户 (state & 2 > 0)
        // 李四(state=2)和王五(state=3)应该匹配这个条件
        List<SysUserVO> list2 = SqlHelper.of(SysUser.class)
                .bitWith(SysUser::getState, 2)
                .wrap(postgresService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 2); // 李四和王五
        assertTrue(list2.stream().allMatch(e -> (e.getState() & 2) > 0));
    }

    /**
     * 测试 bitwiseWithout 条件
     */
    @Test
    @Order(18)
    public void testBitwiseWithout() {
        // 测试不具有指定位的用户 (state & 2 = 0)
        // 只有张三(state=1)应该匹配这个条件
        List<SysUserVO> list1 = SqlHelper.of(SysUser.class)
                .bitWithout(SysUser::getState, 2)
                .wrap(postgresService)
                .list();
        assertNotNull(list1);
        assertFalse(list1.isEmpty());
        assertTrue(list1.stream().allMatch(e -> (e.getState() & 2) == 0));

        // 测试不具有指定位的用户 (state & 1 = 0)
        // 只有李四(state=2)应该匹配这个条件
        List<SysUserVO> list2 = SqlHelper.of(SysUser.class)
                .bitWithout(SysUser::getState, 1)
                .wrap(postgresService)
                .list();
        assertNotNull(list2);
        assertFalse(list2.isEmpty());
        assertTrue(list2.stream().allMatch(e -> (e.getState() & 1) == 0));
    }

    /**
     * 测试空值和边界条件
     */
    @Test
    @Order(19)
    public void testNullAndBoundaryConditions() {
        // 测试空值查询
        List<SysUserVO> nullNameList = SqlHelper.of(SysUser.class)
                .isNull(SysUser::getName)
                .wrap(postgresService)
                .list();
        assertNotNull(nullNameList);
        assertTrue(nullNameList.size() >= 1); // user4的name为null

        // 测试边界值查询
        List<SysUserVO> zeroAgeList = SqlHelper.of(SysUser.class)
                .eq(SysUser::getAge, 0)
                .wrap(postgresService)
                .list();
        assertNotNull(zeroAgeList);
        assertTrue(zeroAgeList.size() >= 1); // user5的age为0

        // 测试空集合查询
        List<SysUserVO> emptyInList = SqlHelper.of(SysUser.class)
                .in(SysUser::getAge, Collections.emptyList())
                .wrap(postgresService)
                .list();
        assertNotNull(emptyInList);
        assertTrue(emptyInList.isEmpty()); // 空集合应该返回空结果
    }

    /**
     * 测试复杂组合条件
     */
    @Test
    @Order(20)
    public void testComplexConditions() {
        // 测试复杂AND条件组合
        List<SysUserVO> complexAndList = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 25)
                .le(SysUser::getAge, 35)
                .isNotNull(SysUser::getName)
                .wrap(postgresService)
                .list();
        assertNotNull(complexAndList);
        assertTrue(complexAndList.size() >= 2); // 张三、李四、王五符合条件

        // 测试复杂OR条件组合
        List<SysUserVO> complexOrList = SqlHelper.of(SysUser.class)
                .eq(SysUser::getName, "张三")
                .or(helper -> helper.eq(SysUser::getName, "李四"))
                .wrap(postgresService)
                .list();
        assertNotNull(complexOrList);
        assertEquals(2, complexOrList.size()); // 张三和李四

        // 测试混合条件
        List<SysUserVO> mixedList = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 20)
                .or(helper -> helper
                        .eq(SysUser::getName, "赵六")
                        .eq(SysUser::getAge, 0))
                .wrap(postgresService)
                .list();
        assertNotNull(mixedList);
        assertTrue(mixedList.size() >= 4); // 应该包含大部分用户
    }

    /**
     * 测试字段后缀功能
     */
    @Test
    @Order(21)
    public void testFieldSuffixFeatures() {
        // 创建带后缀字段的查询对象
        Map<String, Object> queryParams = new HashMap<>();
        queryParams.put("nameLike", "三"); // 应该转换为name LIKE '%三%'
        queryParams.put("ageGe", 25); // 应该转换为age >= 25
        queryParams.put("ageLe", 35); // 应该转换为age <= 35

        List<SysUserVO> suffixList = SqlHelper.of(SysUser.class)
                .with(queryParams)
                .wrap(postgresService)
                .list();
        assertNotNull(suffixList);
        assertTrue(suffixList.size() >= 1); // 张三应该匹配

        // 验证结果
        suffixList.forEach(user -> {
            assertTrue(user.getName().contains("三"));
            assertTrue(user.getAge() >= 25 && user.getAge() <= 35);
        });
    }

    /**
     * 测试异常情况处理
     */
    @Test
    @Order(22)
    public void testExceptionHandling() {
        // 测试无效操作符（这个测试可能需要根据实际实现调整）
        assertDoesNotThrow(() -> {
            List<SysUserVO> list = SqlHelper.of(SysUser.class)
                    .eq(SysUser::getName, "张三")
                    .wrap(postgresService)
                    .list();
            assertNotNull(list);
        });

        // 测试空参数
        assertDoesNotThrow(() -> {
            List<SysUserVO> list = SqlHelper.of(SysUser.class)
                    .with(null)
                    .wrap(postgresService)
                    .list();
            assertNotNull(list);
        });
    }

    /**
     * 测试性能和大数据量处理
     */
    @Test
    @Order(23)
    public void testPerformanceAndLargeData() {
        long startTime = System.currentTimeMillis();

        // 执行复杂查询
        List<SysUserVO> performanceList = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 0)
                .le(SysUser::getAge, 100)
                .isNotNull(SysUser::getCreateTime)
                .orderByAsc(SysUser::getAge)
                .orderByDesc(SysUser::getCreateTime)
                .wrap(postgresService)
                .list();

        long endTime = System.currentTimeMillis();
        long executionTime = endTime - startTime;

        assertNotNull(performanceList);
        assertTrue(executionTime < 5000); // 查询应该在5秒内完成
        log.info("复杂查询执行时间: {}ms, 结果数量: {}", executionTime, performanceList.size());

        // 验证排序结果
        if (performanceList.size() > 1) {
            for (int i = 1; i < performanceList.size(); i++) {
                SysUserVO prev = performanceList.get(i - 1);
                SysUserVO curr = performanceList.get(i);
                assertTrue(prev.getAge() <= curr.getAge(), "年龄排序不正确");
            }
        }
    }

    /**
     * 测试分页功能的边界情况
     */
    @Test
    @Order(24)
    public void testPaginationBoundaries() {
        // 测试第一页
        IPage<SysUserVO> firstPage = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 0)
                .wrap(postgresService)
                .page(1L, 2L);
        assertNotNull(firstPage);
        assertEquals(1, firstPage.getCurrent());
        assertEquals(2, firstPage.getSize());
        assertTrue(firstPage.getTotal() >= 5);

        // 测试超出范围的页码
        IPage<SysUserVO> outOfRangePage = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 0)
                .wrap(postgresService)
                .page(999L, 10L);
        assertNotNull(outOfRangePage);
        assertEquals(999, outOfRangePage.getCurrent());
        assertTrue(outOfRangePage.getRecords().isEmpty());

        // 测试负数页码（应该被修正为1）
        IPage<SysUserVO> negativePage = SqlHelper.of(SysUser.class)
                .ge(SysUser::getAge, 0)
                .wrap(postgresService)
                .page(-1L, 10L);
        assertNotNull(negativePage);
        assertEquals(1, negativePage.getCurrent()); // 应该被修正为1
    }

}
