package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.generate.config.core.CustomConfig;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import lombok.Getter;

import java.util.Map;

/**
 * @author bootystar
 */
@Getter
public class DynamicSqlBuilder extends BaseEnhanceBuilder<DynamicSqlBuilder> {

    {
        selectDTO = new ClassInfo(SqlHelper.class);
        mapperDTO = new ClassInfo(UnmodifiableSqlHelper.class);
    }

    @Override
    public DynamicSqlBuilder withMapSelectDTO() {
        this.selectDTO = new ClassInfo(Map.class);
        return this.getBuilder();
    }

}


