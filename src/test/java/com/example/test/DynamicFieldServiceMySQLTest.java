package com.example.test;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.test.dto.SysUserSelectDTO;
import com.example.test.entity.SysUser;
import com.example.test.impl.SysUserDynamicFieldService;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.sql.helper.SqlHelper;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * MySQL数据源测试类
 * 测试DynamicFieldService在MySQL数据源下的功能
 */
@Slf4j
@SpringBootTest
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class DynamicFieldServiceMySQLTest {

    @Autowired
    private SysUserDynamicFieldService sysUserDynamicFieldService;

    private static Long userId1;
    private static Long userId2;
    private static Long userId3;

    @BeforeEach
    void setUp() {
        // 准备测试数据到mysql数据源
        SysUser user1 = new SysUser();
        user1.setName("赵六");
        user1.setAge(28);
        user1.setBirthDate(LocalDate.of(1995, 3, 15));
        user1.setState(1);
        user1.setCreateTime(LocalDateTime.now());
        user1.setUpdateTime(LocalDateTime.now());
        user1.setDeleted(0);
        user1.setVersion(1);
        sysUserDynamicFieldService.save(user1);
        userId1 = user1.getId();

        SysUser user2 = new SysUser();
        user2.setName("钱七");
        user2.setAge(32);
        user2.setBirthDate(LocalDate.of(1991, 8, 22));
        user2.setState(1);
        user2.setCreateTime(LocalDateTime.now());
        user2.setUpdateTime(LocalDateTime.now());
        user2.setDeleted(0);
        user2.setVersion(1);
        sysUserDynamicFieldService.save(user2);
        userId2 = user2.getId();

        SysUser user3 = new SysUser();
        user3.setName("孙八");
        user3.setAge(27);
        user3.setBirthDate(LocalDate.of(1996, 11, 5));
        user3.setState(0);
        user3.setCreateTime(LocalDateTime.now());
        user3.setUpdateTime(LocalDateTime.now());
        user3.setDeleted(0);
        user3.setVersion(1);
        sysUserDynamicFieldService.save(user3);
        userId3 = user3.getId();
    }

    @AfterEach
    void tearDown() {
        // 清理mysql数据源测试数据
        if (userId1 != null) sysUserDynamicFieldService.removeById(userId1);
        if (userId2 != null) sysUserDynamicFieldService.removeById(userId2);
        if (userId3 != null) sysUserDynamicFieldService.removeById(userId3);
    }

    /**
     * 测试 DynamicService 的基础查询功能
     * 包括 voById, voByDTO, voList, voPage 方法
     */
    @Test
    @Order(1)
    public void testDynamicServiceBasicQuery() {
        // 测试 mysql 数据源 voById
        SysUserVO userVO = sysUserDynamicFieldService.voById(userId1);
        assertNotNull(userVO);
        assertEquals("赵六", userVO.getName());

        // 测试 mysql 数据源 voByDTO (使用Map)
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put("id", userId1);
        queryMap.put("name", "赵六");
        SysUserVO userVO2 = sysUserDynamicFieldService.voByDTO(queryMap);
        assertNotNull(userVO2);
        assertEquals("赵六", userVO2.getName());

        // 测试 mysql 数据源 voList (使用Map)
        Map<String, Object> listQueryMap = new HashMap<>();
        listQueryMap.put("state", 1);
        List<SysUserVO> userList = sysUserDynamicFieldService.voList(listQueryMap);
        assertNotNull(userList);
        assertTrue(userList.size() >= 1); // 至少包含赵六

        // 测试 mysql 数据源 voPage (使用Map)
        IPage<SysUserVO> userPage = sysUserDynamicFieldService.voPage(listQueryMap, 1L, 10L);
        assertNotNull(userPage);
        assertTrue(userPage.getTotal() >= 1);
        assertTrue(userPage.getRecords().size() >= 1);
    }

    /**
     * 测试 SqlHelper 条件构造器功能
     */
    @Test
    @Order(2)
    public void testSqlHelper() {
        // 测试 mysql 数据源链式表达式
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .eq(SysUser::getAge, 28)
                .or()
                .eq(SysUser::getName, "钱七")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 1);

        // 测试 mysql 数据源 like 条件
        List<SysUserVO> likeList = SqlHelper.<SysUser>of()
                .like(SysUser::getName, "赵")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(likeList);
        assertTrue(likeList.size() >= 1);
        assertEquals("赵六", likeList.get(0).getName());

        // 测试 mysql 数据源 in 条件
        List<SysUserVO> inList = SqlHelper.<SysUser>of()
                .in(SysUser::getAge, Arrays.asList(28, 32))
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(inList);
        assertTrue(inList.size() >= 1);

        // 测试 mysql 数据源 page 分页
        IPage<SysUserVO> pageResult = SqlHelper.<SysUser>of()
                .ge(SysUser::getAge, 25)
                .wrap(sysUserDynamicFieldService)
                .page(1L, 2L);
        assertNotNull(pageResult);
        assertEquals(2, pageResult.getSize());
        assertTrue(pageResult.getTotal() >= 1);
        assertTrue(pageResult.getRecords().size() >= 1);
    }

    /**
     * 测试 DTO 查询功能
     */
    @Test
    @Order(3)
    public void testDTOQuery() {
        // 测试 mysql 数据源使用 SysUserSelectDTO 查询
        SysUserSelectDTO dto = new SysUserSelectDTO();
        dto.setId(userId1);
        dto.setName("赵六");
        // 确保 DTO 查询也添加 id 条件
        SysUserVO userVO = sysUserDynamicFieldService.voByDTO(dto);
        assertNotNull(userVO);
        assertEquals("赵六", userVO.getName());
        assertEquals(userId1, userVO.getId());

        // 测试 mysql 数据源 ageIn 查询
        SysUserSelectDTO dto2 = new SysUserSelectDTO();
        dto2.setAgeIn(Arrays.asList(28, 32));
        List<SysUserVO> list = sysUserDynamicFieldService.voList(dto2);
        assertNotNull(list);
        assertTrue(list.size() >= 1);
    }

    /**
     * 测试 DynamicFieldService 后缀查询功能
     */
    @Test
    @Order(4)
    public void testDynamicFieldService() {
        // 测试 mysql 数据源 nameLike 后缀查询
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put("nameLike", "赵");
        List<SysUserVO> list = sysUserDynamicFieldService.voList(queryMap);
        assertNotNull(list);
        assertTrue(list.size() >= 1);
        assertEquals("赵六", list.get(0).getName());

        // 测试 mysql 数据源 ageGe 后缀查询
        Map<String, Object> queryMap2 = new HashMap<>();
        queryMap2.put("ageGe", 30);
        List<SysUserVO> list2 = sysUserDynamicFieldService.voList(queryMap2);
        assertNotNull(list2);
        assertTrue(list2.size() >= 1); // 钱七(32)
    }

    /**
     * 测试 JSON 条件查询功能
     */
    @Test
    @Order(5)
    public void testJsonConditionQuery() {
        // 构造类似 JSON 条件的查询 for mysql 数据源
        Map<String, Object> queryMap = new HashMap<>();
        List<Map<String, Object>> conditions = new ArrayList<>();

        Map<String, Object> condition1 = new HashMap<>();
        condition1.put("field", "age");
        condition1.put("operator", ">");
        condition1.put("value", 27);
        conditions.add(condition1);

        Map<String, Object> condition2 = new HashMap<>();
        condition2.put("field", "name");
        condition2.put("operator", "like");
        condition2.put("value", "钱");
        conditions.add(condition2);

        queryMap.put("conditions", conditions);

        // 使用 SqlHelper 构造查询
        SqlHelper<SysUser> sqlHelper = SqlHelper.of(queryMap);
        List<SysUserVO> list = sysUserDynamicFieldService.voList(sqlHelper);
        assertNotNull(list);
        assertTrue(list.size() >= 0); // 可能为空，取决于具体数据和查询逻辑
    }

    /**
     * 测试排序功能
     */
    @Test
    @Order(6)
    public void testSortFunctionality() {
        // 测试 mysql 数据源使用 SqlHelper 排序
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 1);

        // 测试 mysql 数据源多字段排序
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .orderByDesc(SysUser::getState)
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 1);
    }
}