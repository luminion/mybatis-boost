package io.github.bootystar.mybatisplus.generate.handler.support;

import com.baomidou.mybatisplus.generator.config.GlobalConfig;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.rules.DbColumnType;
import com.baomidou.mybatisplus.generator.config.rules.IColumnType;
import com.baomidou.mybatisplus.generator.type.ITypeConvertHandler;
import com.baomidou.mybatisplus.generator.type.TypeRegistry;
import lombok.Setter;
import org.apache.ibatis.type.JdbcType;

/**
 * 类型转换处理器
 *
 * @author bootystar
 */
@Setter
public class TypeConvertHandler implements ITypeConvertHandler {
    public boolean byte2Integer;
    public boolean short2Integer;

    @Override
    public IColumnType convert(GlobalConfig globalConfig, TypeRegistry typeRegistry, TableField.MetaInfo metaInfo) {
        if (JdbcType.TINYINT == metaInfo.getJdbcType() && byte2Integer) {
            return DbColumnType.INTEGER;
        }
        if (JdbcType.SMALLINT == metaInfo.getJdbcType() && short2Integer) {
            return DbColumnType.INTEGER;
        }
        return typeRegistry.getColumnType(metaInfo);
    }
}
