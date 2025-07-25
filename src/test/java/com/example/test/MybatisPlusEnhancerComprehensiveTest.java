package com.example.test;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.example.test.dto.SysUserSelectDTO;
import com.example.test.entity.SysUser;
import com.example.test.impl.SysUserDynamicFieldService;
import com.example.test.impl.SysUserDynamicSqlService;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.jdbc.Sql;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * MyBatis Plus Enhancer 综合测试类
 * 测试 README 文档中提到的所有功能点
 */
@Slf4j
@SpringBootTest
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class MybatisPlusEnhancerComprehensiveTest {

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
     * 测试 DynamicService 的基础查询功能
     * 包括 voById, voByDTO, voList, voPage 方法
     */
    @Test
    @Order(1)
    public void testDynamicServiceBasicQuery() {
        // 测试 postgresql 数据源 voById
        SysUserVO userVO = sysUserDynamicSqlService.voById(userId1.toString());
        assertNotNull(userVO);
        assertEquals("张三", userVO.getName());

        // 测试 postgresql 数据源 voByDTO (使用Map)
        Map<String, Object> queryMap = new HashMap<>();
        queryMap.put("name", "张三");
        SysUserVO userVO2 = sysUserDynamicSqlService.voByDTO(queryMap);
        assertNotNull(userVO2);
        assertEquals("张三", userVO2.getName());

        // 测试 postgresql 数据源 voList (使用Map)
        Map<String, Object> listQueryMap = new HashMap<>();
        listQueryMap.put("state", 1);
        List<SysUserVO> userList = sysUserDynamicSqlService.voList(listQueryMap);
        assertNotNull(userList);
        assertTrue(userList.size() >= 2); // 至少包含张三和李四

        // 测试 postgresql 数据源 voPage (使用Map)
        IPage<SysUserVO> userPage = sysUserDynamicSqlService.voPage(listQueryMap, 1L, 10L);
        assertNotNull(userPage);
        assertTrue(userPage.getTotal() >= 2);
        assertTrue(userPage.getRecords().size() >= 2);
        
        // 测试 mysql 数据源 voById
        SysUserVO userVO3 = sysUserDynamicFieldService.voById(userId4.toString());
        assertNotNull(userVO3);
        assertEquals("赵六", userVO3.getName());

        // 测试 mysql 数据源 voByDTO (使用Map)
        Map<String, Object> queryMap2 = new HashMap<>();
        queryMap2.put("name", "赵六");
        SysUserVO userVO4 = sysUserDynamicFieldService.voByDTO(queryMap2);
        assertNotNull(userVO4);
        assertEquals("赵六", userVO4.getName());

        // 测试 mysql 数据源 voList (使用Map)
        Map<String, Object> listQueryMap2 = new HashMap<>();
        listQueryMap2.put("state", 1);
        List<SysUserVO> userList2 = sysUserDynamicFieldService.voList(listQueryMap2);
        assertNotNull(userList2);
        assertTrue(userList2.size() >= 2); // 至少包含赵六和钱七

        // 测试 mysql 数据源 voPage (使用Map)
        IPage<SysUserVO> userPage2 = sysUserDynamicFieldService.voPage(listQueryMap2, 1L, 10L);
        assertNotNull(userPage2);
        assertTrue(userPage2.getTotal() >= 2);
        assertTrue(userPage2.getRecords().size() >= 2);
    }

    /**
     * 测试 SqlHelper 条件构造器功能
     */
    @Test
    @Order(2)
    public void testSqlHelper() {
        // 测试 postgresql 数据源链式表达式
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .eq(SysUser::getAge, 25)
                .or()
                .eq(SysUser::getName, "李四")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertEquals(2, list.size());

        // 测试 postgresql 数据源 like 条件
        List<SysUserVO> likeList = SqlHelper.<SysUser>of()
                .like(SysUser::getName, "张")
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(likeList);
        assertEquals(1, likeList.size());
        assertEquals("张三", likeList.get(0).getName());

        // 测试 postgresql 数据源 in 条件
        List<SysUserVO> inList = SqlHelper.<SysUser>of()
                .in(SysUser::getAge, Arrays.asList(25, 30))
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(inList);
        assertEquals(2, inList.size());

        // 测试 postgresql 数据源 page 分页
        IPage<SysUserVO> pageResult = SqlHelper.<SysUser>of()
                .ge(SysUser::getAge, 25)
                .wrap(sysUserDynamicSqlService)
                .page(1L, 2L);
        assertNotNull(pageResult);
        assertEquals(2, pageResult.getSize());
        assertTrue(pageResult.getTotal() >= 2);
        
        // 测试 mysql 数据源链式表达式
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .eq(SysUser::getAge, 28)
                .or()
                .eq(SysUser::getName, "钱七")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list2);
        assertEquals(2, list2.size());

        // 测试 mysql 数据源 like 条件
        List<SysUserVO> likeList2 = SqlHelper.<SysUser>of()
                .like(SysUser::getName, "赵")
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(likeList2);
        assertEquals(1, likeList2.size());
        assertEquals("赵六", likeList2.get(0).getName());

        // 测试 mysql 数据源 in 条件
        List<SysUserVO> inList2 = SqlHelper.<SysUser>of()
                .in(SysUser::getAge, Arrays.asList(28, 32))
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(inList2);
        assertEquals(2, inList2.size());

        // 测试 mysql 数据源 page 分页
        IPage<SysUserVO> pageResult2 = SqlHelper.<SysUser>of()
                .ge(SysUser::getAge, 25)
                .wrap(sysUserDynamicFieldService)
                .page(1L, 2L);
        assertNotNull(pageResult2);
        assertEquals(2, pageResult2.getSize());
        assertTrue(pageResult2.getTotal() >= 2);
    }

    /**
     * 测试 DTO 查询功能
     */
    @Test
    @Order(3)
    public void testDTOQuery() {
        // 测试 postgresql 数据源使用 SysUserSelectDTO 查询
        SysUserSelectDTO dto = new SysUserSelectDTO();
        dto.setName("张三");
        SysUserVO userVO = sysUserDynamicSqlService.voByDTO(dto);
        assertNotNull(userVO);
        assertEquals("张三", userVO.getName());

        // 测试 postgresql 数据源 ageIn 查询
        SysUserSelectDTO dto2 = new SysUserSelectDTO();
        dto2.setAgeIn(Arrays.asList(25, 30));
        List<SysUserVO> list = sysUserDynamicSqlService.voList(dto2);
        assertNotNull(list);
        assertEquals(2, list.size());
        
        // 测试 mysql 数据源使用 SysUserSelectDTO 查询
        SysUserSelectDTO dto3 = new SysUserSelectDTO();
        dto3.setName("赵六");
        SysUserVO userVO2 = sysUserDynamicFieldService.voByDTO(dto3);
        assertNotNull(userVO2);
        assertEquals("赵六", userVO2.getName());

        // 测试 mysql 数据源 ageIn 查询
        SysUserSelectDTO dto4 = new SysUserSelectDTO();
        dto4.setAgeIn(Arrays.asList(28, 32));
        List<SysUserVO> list2 = sysUserDynamicFieldService.voList(dto4);
        assertNotNull(list2);
        assertEquals(2, list2.size());
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
        assertEquals(1, list.size());
        assertEquals("赵六", list.get(0).getName());

        // 测试 mysql 数据源 ageGe 后缀查询
        Map<String, Object> queryMap2 = new HashMap<>();
        queryMap2.put("ageGe", 30);
        List<SysUserVO> list2 = sysUserDynamicFieldService.voList(queryMap2);
        assertNotNull(list2);
        assertTrue(list2.size() >= 2); // 钱七(32)和可能的其他用户
    }

    /**
     * 测试 JSON 条件查询功能
     */
    @Test
    @Order(5)
    public void testJsonConditionQuery() {
        // 构造类似 JSON 条件的查询 for postgresql 数据源
        Map<String, Object> queryMap = new HashMap<>();
        List<Map<String, Object>> conditions = new ArrayList<>();

        Map<String, Object> condition1 = new HashMap<>();
        condition1.put("field", "age");
        condition1.put("operator", ">");
        condition1.put("value", 25);
        conditions.add(condition1);

        Map<String, Object> condition2 = new HashMap<>();
        condition2.put("field", "name");
        condition2.put("operator", "like");
        condition2.put("value", "王");
        conditions.add(condition2);

        queryMap.put("conditions", conditions);

        // 使用 SqlHelper 构造查询
        SqlHelper<SysUser> sqlHelper = SqlHelper.of(queryMap);
        List<SysUserVO> list = sysUserDynamicSqlService.voList(sqlHelper);
        assertNotNull(list);
        
        // 构造类似 JSON 条件的查询 for mysql 数据源
        Map<String, Object> queryMap2 = new HashMap<>();
        List<Map<String, Object>> conditions2 = new ArrayList<>();

        Map<String, Object> condition3 = new HashMap<>();
        condition3.put("field", "age");
        condition3.put("operator", ">");
        condition3.put("value", 27);
        conditions2.add(condition3);

        Map<String, Object> condition4 = new HashMap<>();
        condition4.put("field", "name");
        condition4.put("operator", "like");
        condition4.put("value", "钱");
        conditions2.add(condition4);

        queryMap2.put("conditions", conditions2);

        // 使用 SqlHelper 构造查询
        SqlHelper<SysUser> sqlHelper2 = SqlHelper.of(queryMap2);
        List<SysUserVO> list2 = sysUserDynamicFieldService.voList(sqlHelper2);
        assertNotNull(list2);
    }

    /**
     * 测试排序功能
     */
    @Test
    @Order(6)
    public void testSortFunctionality() {
        // 测试 postgresql 数据源使用 SqlHelper 排序
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list);
        assertTrue(list.size() >= 3);

        // 测试 postgresql 数据源多字段排序
        List<SysUserVO> list2 = SqlHelper.<SysUser>of()
                .orderByDesc(SysUser::getState)
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicSqlService)
                .list();
        assertNotNull(list2);
        assertTrue(list2.size() >= 3);
        
        // 测试 mysql 数据源使用 SqlHelper 排序
        List<SysUserVO> list3 = SqlHelper.<SysUser>of()
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list3);
        assertTrue(list3.size() >= 3);

        // 测试 mysql 数据源多字段排序
        List<SysUserVO> list4 = SqlHelper.<SysUser>of()
                .orderByDesc(SysUser::getState)
                .orderByAsc(SysUser::getAge)
                .wrap(sysUserDynamicFieldService)
                .list();
        assertNotNull(list4);
        assertTrue(list4.size() >= 3);
    }
}